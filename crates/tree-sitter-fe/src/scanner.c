#include "tree_sitter/parser.h"
#include <stdbool.h>

// Must match the order in grammar.js externals array
enum TokenType {
  AUTOMATIC_SEMICOLON,
  BLOCK_COMMENT_CONTENT,
  BLOCK_COMMENT_END,
  GENERIC_OPEN,
  COMPARISON_LT,
  QUOTE_KEYWORD,
};

void *tree_sitter_fe_external_scanner_create(void) { return NULL; }
void tree_sitter_fe_external_scanner_destroy(void *payload) {
  (void)payload;
}
unsigned tree_sitter_fe_external_scanner_serialize(void *payload, char *buffer) {
  (void)payload;
  (void)buffer;
  return 0;
}
void tree_sitter_fe_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  (void)payload;
  (void)buffer;
  (void)length;
}

static void advance(TSLexer *lexer) { lexer->advance(lexer, false); }
static void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

static bool is_continuation_after_newline(int32_t c) {
  switch (c) {
    case '.':
    case '+':
    case '/':
    case '%':
    case '|':
    case '&':
    case '^':
    case '>':
    case '=':
      return true;
    default:
      return false;
  }
}

// Skip a block comment (after consuming /*). Returns true if successfully skipped.
static bool skip_block_comment(TSLexer *lexer) {
  int depth = 1;
  while (depth > 0 && !lexer->eof(lexer)) {
    if (lexer->lookahead == '*') {
      skip(lexer);
      if (!lexer->eof(lexer) && lexer->lookahead == '/') {
        skip(lexer);
        depth--;
      }
      continue;
    }
    if (lexer->lookahead == '/') {
      skip(lexer);
      if (!lexer->eof(lexer) && lexer->lookahead == '*') {
        skip(lexer);
        depth++;
      }
      continue;
    }
    skip(lexer);
  }
  return depth == 0;
}

// Scan for automatic semicolon insertion.
// Returns true to emit a zero-width semicolon, false to let the parser
// continue without one.
//
// After finding a newline (or EOF/closing-brace), we peek at the first
// non-whitespace token on the *next* line. If it is a continuation token,
// we suppress the semicolon so
// that multi-line expressions work correctly:
//
//   x
//   + y      <- no semicolon after x
//   || z     <- no semicolon after + y
//   .y()     <- no semicolon after x
//   .z()     <- no semicolon after .y()
//
static bool scan_automatic_semicolon(TSLexer *lexer) {
  lexer->result_symbol = AUTOMATIC_SEMICOLON;
  lexer->mark_end(lexer);

  bool saw_newline = false;

  for (;;) {
    if (lexer->eof(lexer)) return true;

    int32_t c = lexer->lookahead;

    if (c == '\n') {
      saw_newline = true;
      skip(lexer);
      continue;
    }

    // Closing brace -- statement ends before }
    if (c == '}') return true;

    // Skip non-newline whitespace
    if (c == ' ' || c == '\t' || c == '\r') {
      skip(lexer);
      continue;
    }

    // Handle comments
    if (c == '/') {
      skip(lexer);
      if (lexer->lookahead == '/') {
        // Line comment -- skip to newline
        while (!lexer->eof(lexer) && lexer->lookahead != '\n') {
          skip(lexer);
        }
        continue;
      }
      if (lexer->lookahead == '*') {
        // Block comment -- skip through it
        skip(lexer);
        skip_block_comment(lexer);
        continue;
      }
      // Just `/` -- division operator continuation
      return false;
    }

    // We've found a non-whitespace, non-comment character.
    if (!saw_newline) {
      // Still on the same line -- no semicolon (more expression follows)
      return false;
    }

    // We're past a newline. Check for continuation tokens.
    if (is_continuation_after_newline(c)) return false;

    if (c == '!') {
      // `!=` is a continuation; unary `!` starts a new expression.
      advance(lexer);
      return lexer->lookahead != '=';
    }

    if (c == '-') {
      // `-=` is a continuation; unary/binary `-` starts a new expression.
      advance(lexer);
      return lexer->lookahead != '=';
    }

    if (c == '*') {
      // Bare `*` starts a dereference statement. `*=` and `**` are
      // unambiguously infix continuations.
      advance(lexer);
      return lexer->lookahead != '=' && lexer->lookahead != '*';
    }

    if (c == '<') {
      // Only `<=` and `<<=` are continuations.
      // Bare `<` and bare `<<` start a new expression boundary.
      advance(lexer);
      if (lexer->lookahead == '=') return false;

      if (lexer->lookahead == '<') {
        advance(lexer);
        return lexer->lookahead != '=';
      }

      return true;
    }

    // Any other token after a newline -- insert semicolon
    return true;
  }
}


// Advance past a string literal, starting at its opening quote, so that its
// contents are never read as code. An escape is `\` and the character after it,
// which keeps `"\\"` from swallowing the closing quote.
static void advance_over_string_literal(TSLexer *lexer) {
  advance(lexer);  // consume the opening '"'
  while (!lexer->eof(lexer)) {
    if (lexer->lookahead == '\\') {
      advance(lexer);
      if (lexer->eof(lexer)) return;
      advance(lexer);
      continue;
    }
    bool is_end = lexer->lookahead == '"';
    advance(lexer);
    if (is_end) return;
  }
}

// Advance past a comment if one starts at the `/` the caller is looking at, so
// that its text is never read as code. Consumes the `/` either way, since a lone
// `/` is division and needs no further handling.
static void advance_over_comment(TSLexer *lexer) {
  advance(lexer);  // consume the '/'
  if (lexer->lookahead == '/') {
    while (!lexer->eof(lexer) && lexer->lookahead != '\n') advance(lexer);
    return;
  }
  if (lexer->lookahead != '*') return;
  advance(lexer);
  int depth = 1;
  while (depth > 0 && !lexer->eof(lexer)) {
    if (lexer->lookahead == '*') {
      advance(lexer);
      if (!lexer->eof(lexer) && lexer->lookahead == '/') {
        advance(lexer);
        depth--;
      }
      continue;
    }
    if (lexer->lookahead == '/') {
      advance(lexer);
      if (!lexer->eof(lexer) && lexer->lookahead == '*') {
        advance(lexer);
        depth++;
      }
      continue;
    }
    advance(lexer);
  }
}

// Whether a `<<` opens generic arguments whose first argument is a qualified
// path, as in `Wrapped<<M as Model>::Point>`, rather than being a left shift.
//
// The caller has consumed the first `<` and marked the token end, so this only
// looks ahead. It scans the second `<` to its matching `>` and requires `::` to
// follow, the same prefix rule the compiler's parser uses: no shift operand
// continues with `>::`, so `value << <T as Model>::BITS > limit` stays a shift
// because the `>` that closes its angle nesting is followed by ` limit`.
//
// Nesting depths mirror the single-`<` scan below, so a `)`, `]`, `}` or `;`
// that closes something never opened settles it as a shift. String literals and
// comments are skipped whole: the scan runs on to the end of the enclosing block
// before giving up, and a `>::` in a message or a comment there is text, not the
// close of a qualified path.
static bool scan_qualified_path_after_lshift(TSLexer *lexer) {
  if (lexer->lookahead != '<') return false;
  advance(lexer);  // consume the second '<'
  // `<<=` is a shift-assign; no type starts with `=`.
  if (lexer->lookahead == '=') return false;

  int angle_depth = 1;
  int paren_depth = 0;
  int bracket_depth = 0;
  int brace_depth = 0;

  while (!lexer->eof(lexer)) {
    switch (lexer->lookahead) {
      case '(': paren_depth++; advance(lexer); continue;
      case ')':
        if (paren_depth == 0) return false;
        paren_depth--;
        advance(lexer);
        continue;
      case '[': bracket_depth++; advance(lexer); continue;
      case ']':
        if (bracket_depth == 0) return false;
        bracket_depth--;
        advance(lexer);
        continue;
      case '{': brace_depth++; advance(lexer); continue;
      case '}':
        if (brace_depth == 0) return false;
        brace_depth--;
        advance(lexer);
        continue;
      case ';':
        if (bracket_depth == 0 && brace_depth == 0) return false;
        advance(lexer);
        continue;
      case '<':
        if (paren_depth == 0 && bracket_depth == 0 && brace_depth == 0) {
          angle_depth++;
        }
        advance(lexer);
        continue;
      case '>':
        if (paren_depth != 0 || bracket_depth != 0 || brace_depth != 0) {
          advance(lexer);
          continue;
        }
        advance(lexer);
        if (--angle_depth > 0) continue;
        // The qualified path is closed; `::` after it is what distinguishes it
        // from a shift.
        while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
          advance(lexer);
        }
        if (lexer->lookahead != ':') return false;
        advance(lexer);
        return lexer->lookahead == ':';
      case '"': advance_over_string_literal(lexer); continue;
      case '/': advance_over_comment(lexer); continue;
      default:
        advance(lexer);
        continue;
    }
  }
  return false;
}

// Whether what follows the `>` that would close an expression path's generic
// arguments lets that path end there. It does not when it starts an operand
// and can never continue an expression after one, as the `0` in
// `N < 8, M > 0`, or when it is the `=` of a `>=`, as in `N < 8, M >= 1`: the
// `<` and `>` are comparisons then. This mirrors `cannot_follow_path_expr` in
// the compiler's parser (`crates/parser/src/parser/path.rs`). The caller has
// consumed the `>`; this only looks ahead on the same line.
static bool can_follow_generic_path_expr(TSLexer *lexer) {
  while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
    advance(lexer);
  }
  int32_t c = lexer->lookahead;
  if (c == '=') {
    advance(lexer);
    // `=>` ends a match arm pattern; any other `=` makes a `>=`.
    return lexer->lookahead == '>';
  }
  if (c == '!') {
    advance(lexer);
    return lexer->lookahead == '=';
  }
  if (c == '~' || c == '"' || (c >= '0' && c <= '9')) return false;
  if (c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
    // A word: only `as` continues an expression, as a cast.
    char word[3];
    int len = 0;
    while (lexer->lookahead == '_' || (lexer->lookahead >= 'a' && lexer->lookahead <= 'z') ||
           (lexer->lookahead >= 'A' && lexer->lookahead <= 'Z') ||
           (lexer->lookahead >= '0' && lexer->lookahead <= '9')) {
      if (len < 3) word[len] = (char)lexer->lookahead;
      len++;
      advance(lexer);
    }
    return len == 2 && word[0] == 'a' && word[1] == 's';
  }
  return true;
}

static bool is_ident_char(int32_t c) {
  return c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
         (c >= '0' && c <= '9');
}

// Consumes `word` when it is the whole identifier at the lookahead.
static bool scan_word(TSLexer *lexer, const char *word) {
  for (const char *c = word; *c; c++) {
    if (lexer->lookahead != *c) return false;
    advance(lexer);
  }
  return !is_ident_char(lexer->lookahead);
}

// Lookahead after mark_end must never skip: skipping would move the start of
// the already marked quote/generic token. The opening /* is consumed here.
static bool advance_quote_block_comment(TSLexer *lexer) {
  int depth = 1;
  while (depth > 0 && !lexer->eof(lexer)) {
    if (lexer->lookahead == '*') {
      advance(lexer);
      if (lexer->lookahead == '/') { advance(lexer); depth--; }
    } else if (lexer->lookahead == '/') {
      advance(lexer);
      if (lexer->lookahead == '*') { advance(lexer); depth++; }
    } else {
      advance(lexer);
    }
  }
  return depth == 0;
}

// Ignore trivia tokens, but not a standalone newline, just like the Rust
// parser's current_kind_same_line. A newline inside a block comment remains
// part of that single comment token.
static bool skip_quote_trivia(TSLexer *lexer, bool allow_newlines) {
  for (;;) {
    while (lexer->lookahead == ' ' || lexer->lookahead == '\t' || lexer->lookahead == '\r' || (allow_newlines && lexer->lookahead == '\n')) advance(lexer);
    if (lexer->lookahead != '/') return allow_newlines || lexer->lookahead != '\n';
    advance(lexer);
    if (lexer->lookahead == '/') {
      while (!lexer->eof(lexer) && lexer->lookahead != '\n') advance(lexer);
      if (!allow_newlines) return false;
      continue;
    }
    if (lexer->lookahead != '*') return false; // A division operator is not trivia.
    advance(lexer);
    if (!advance_quote_block_comment(lexer)) return false;
  }
}

static bool scan_quote_keyword(TSLexer *lexer) {
  if (!scan_word(lexer, "quote")) return false;
  lexer->mark_end(lexer);
  return skip_quote_trivia(lexer, false) && lexer->lookahead == '{';
}


bool tree_sitter_fe_external_scanner_scan(void *payload, TSLexer *lexer,
                                          const bool *valid_symbols) {
  (void)payload;

  // Skip if in error recovery mode (all symbols valid)
  if (valid_symbols[AUTOMATIC_SEMICOLON] &&
      valid_symbols[BLOCK_COMMENT_CONTENT] &&
      valid_symbols[BLOCK_COMMENT_END] &&
      valid_symbols[GENERIC_OPEN] &&
      valid_symbols[COMPARISON_LT]) {
    return false;
  }

  // Try automatic semicolon first, but if it returns false and other tokens
  // are also valid, fall through to check them.
  if (valid_symbols[AUTOMATIC_SEMICOLON]) {
    if (scan_automatic_semicolon(lexer)) {
      return true;
    }
    // Automatic semicolon not needed -- fall through to check other tokens
  }

  if (valid_symbols[QUOTE_KEYWORD]) {
    while (lexer->lookahead == ' ' || lexer->lookahead == '\t' ||
           lexer->lookahead == '\r' || lexer->lookahead == '\n') {
      skip(lexer);
    }
    if (lexer->lookahead == 'q') {
      lexer->result_symbol = QUOTE_KEYWORD;
      return scan_quote_keyword(lexer);
    }
  }


  // Disambiguate '<': generic open vs comparison less-than.
  // When both are valid, try generic first (lookahead for matching '>').
  // If no matching '>' found, fall back to comparison.
  if ((valid_symbols[GENERIC_OPEN] || valid_symbols[COMPARISON_LT]) &&
      !lexer->eof(lexer)) {
    // Skip whitespace before checking for '<'
    while (!lexer->eof(lexer) &&
           (lexer->lookahead == ' ' || lexer->lookahead == '\t' ||
            lexer->lookahead == '\r' || lexer->lookahead == '\n')) {
      skip(lexer);
    }
    if (lexer->lookahead == '<') {
      // Peek ahead: consume '<' and check next character to determine
      // if this is a multi-character operator (<=, <<, <<=).
      lexer->mark_end(lexer);
      advance(lexer);  // consume '<'
      int32_t next = lexer->lookahead;

      if (next == '=') {
        // This is <= -- let the internal lexer handle it.
        return false;
      }

      if (next == '<') {
        // Could be << (shift), <<= (shift-assign), or nested generics <<T as...
        if (valid_symbols[GENERIC_OPEN]) {
          lexer->mark_end(lexer);  // mark end after first '<'
          // In a type position COMPARISON_LT is not valid, so nothing else the
          // '<<' could be: this is the start of a qualified path such as
          // <<T as Trait>::Item as ...>.
          if (!valid_symbols[COMPARISON_LT]) {
            lexer->result_symbol = GENERIC_OPEN;
            return true;
          }
          // In an expression position both are valid, so apply the same rule
          // the compiler's parser uses and require a qualified path after the
          // '<<'. Without this, `Wrapped<<M as Model>::Point>::new(p)` lexes a
          // shift and fails to parse even though the compiler accepts it.
          if (scan_qualified_path_after_lshift(lexer)) {
            lexer->result_symbol = GENERIC_OPEN;
            return true;
          }
        }
        // Otherwise let the internal lexer handle << / <<=
        return false;
      }

      // Single '<' -- decide between generic and comparison.
      lexer->mark_end(lexer);  // mark end after '<'

      if (valid_symbols[GENERIC_OPEN]) {
        // Continue scanning ahead to find matching '>' for generic.
        // We already consumed '<'; now lookahead for balanced '>'.
        // Track angle depth, plus paren/bracket/brace depth so we don't
        // mistake a '>' inside nested delimiters for the closing angle.
        int angle_depth = 1;
        int paren_depth = 0;
        int bracket_depth = 0;
        int brace_depth = 0;
        bool is_generic = false;
        // Track last non-whitespace char at top level (all depths 0) to
        // distinguish `Foo<T, {expr}>` ('{' after ',') from `if x < y {` ('{' after identifier).
        int32_t last_top_char = '<';  // Starts as '<' since we just consumed it.

        while (!lexer->eof(lexer)) {
          int32_t c = lexer->lookahead;

          switch (c) {
            case '(':
              paren_depth++;
              advance(lexer);
              continue;
            case ')':
              if (paren_depth == 0) goto not_generic;
              paren_depth--;
              advance(lexer);
              continue;
            case '[':
              bracket_depth++;
              advance(lexer);
              continue;
            case ']':
              if (bracket_depth == 0) goto not_generic;
              bracket_depth--;
              advance(lexer);
              continue;
            case '{':
              // At top level (brace_depth 0), '{' is only valid in generics
              // when it starts a const-generic block expression, i.e. after
              // '<' or ','.  If it follows an identifier/)/] it's a block body
              // (e.g. `if x < y { ... }`).
              if (brace_depth == 0 && last_top_char != '<' && last_top_char != ',') {
                goto not_generic;
              }
              brace_depth++;
              advance(lexer);
              continue;
            case '}':
              if (brace_depth == 0) goto not_generic;
              brace_depth--;
              advance(lexer);
              continue;
            case ';':
              // Semicolons are only valid inside brackets (array types)
              // or braces (const generic blocks)
              if (bracket_depth == 0 && brace_depth == 0) goto not_generic;
              advance(lexer);
              continue;
            case '|':
              advance(lexer);
              // || at top level is a boolean operator -- can't be in generics
              if (paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 &&
                  lexer->lookahead == '|') goto not_generic;
              continue;
            case '&':
              advance(lexer);
              // && at top level is a boolean operator -- can't be in generics
              if (paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 &&
                  lexer->lookahead == '&') goto not_generic;
              continue;
            case '=':
              advance(lexer);
              // => at top level is a match arm separator -- can't be in generics
              // (= alone IS valid: assoc type args like Item = T)
              if (paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 &&
                  lexer->lookahead == '>') goto not_generic;
              continue;
            case '.':
              advance(lexer);
              // .. at top level is a range operator -- can't be in generics
              if (paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 &&
                  lexer->lookahead == '.') goto not_generic;
              continue;
            case '<':
              if (paren_depth == 0 && bracket_depth == 0 && brace_depth == 0) {
                angle_depth++;
                last_top_char = '<';
                advance(lexer);
                if (lexer->lookahead == '<') goto not_generic;
              } else {
                advance(lexer);
              }
              continue;
            case '>':
              if (paren_depth == 0 && bracket_depth == 0 && brace_depth == 0) {
                angle_depth--;
                if (angle_depth == 0) {
                  // In an expression both are valid, so apply the compiler's
                  // rule: what follows the '>' must be able to follow a path.
                  advance(lexer);
                  is_generic = !valid_symbols[COMPARISON_LT] ||
                               can_follow_generic_path_expr(lexer);
                  goto done_scanning;
                }
              }
              advance(lexer);
              continue;
            case ' ': case '\t': case '\r': case '\n':
              advance(lexer);
              continue;
            case '/':
              advance(lexer);
              if (lexer->lookahead == '/') {
                while (!lexer->eof(lexer) && lexer->lookahead != '\n') advance(lexer);
                continue;
              }
              if (lexer->lookahead == '*') {
                advance(lexer);
                int cd = 1;
                while (cd > 0 && !lexer->eof(lexer)) {
                  if (lexer->lookahead == '*') { advance(lexer); if (!lexer->eof(lexer) && lexer->lookahead == '/') { advance(lexer); cd--; } continue; }
                  if (lexer->lookahead == '/') { advance(lexer); if (!lexer->eof(lexer) && lexer->lookahead == '*') { advance(lexer); cd++; } continue; }
                  advance(lexer);
                }
                continue;
              }
              // '/' alone -- division, still valid inside generics
              if (paren_depth == 0 && bracket_depth == 0 && brace_depth == 0) last_top_char = '/';
              continue;
            default:
              if (paren_depth == 0 && bracket_depth == 0 && brace_depth == 0) last_top_char = c;
              advance(lexer);
              continue;
          }
        }

        not_generic:
        done_scanning:

        if (is_generic) {
          lexer->result_symbol = GENERIC_OPEN;
          return true;
        }
      }

      // Not a generic -- emit comparison '<' if valid
      if (valid_symbols[COMPARISON_LT]) {
        lexer->result_symbol = COMPARISON_LT;
        return true;
      }
    }
  }

  // Handle block comment content and end
  if (valid_symbols[BLOCK_COMMENT_CONTENT] || valid_symbols[BLOCK_COMMENT_END]) {
    int depth = 1;
    bool has_content = false;

    while (depth > 0 && !lexer->eof(lexer)) {
      if (lexer->lookahead == '/') {
        advance(lexer);
        if (lexer->lookahead == '*') {
          advance(lexer);
          depth++;
          has_content = true;
          continue;
        }
        has_content = true;
        continue;
      }

      if (lexer->lookahead == '*') {
        if (depth == 1) {
          if (has_content) {
            lexer->result_symbol = BLOCK_COMMENT_CONTENT;
            return true;
          }
          advance(lexer);
          if (lexer->lookahead == '/') {
            advance(lexer);
            lexer->result_symbol = BLOCK_COMMENT_END;
            return true;
          }
          has_content = true;
          continue;
        }

        advance(lexer);
        if (lexer->lookahead == '/') {
          advance(lexer);
          depth--;
          has_content = true;
          continue;
        }
        has_content = true;
        continue;
      }

      advance(lexer);
      has_content = true;
    }

    if (has_content) {
      lexer->result_symbol = BLOCK_COMMENT_CONTENT;
      return true;
    }
    if (depth == 0) {
      lexer->result_symbol = BLOCK_COMMENT_END;
      return true;
    }
  }

  return false;
}
