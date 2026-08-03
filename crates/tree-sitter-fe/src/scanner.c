#include "tree_sitter/parser.h"
#include <stdbool.h>

// Must match the order in grammar.js externals array
enum TokenType {
  AUTOMATIC_SEMICOLON,
  BLOCK_COMMENT_CONTENT,
  BLOCK_COMMENT_END,
  GENERIC_OPEN,
  COMPARISON_LT,
  VIEW_MODE_PREFIX,
  VIEW_MODE_ONLY,
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
    case '*':
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

// Consume a block comment after `/*`. Lookahead performed after a token's
// `mark_end` must use `advance`, not `skip`: skipped bytes move the token start
// and can collapse an already-marked token to a zero-width range.
static bool consume_block_comment(TSLexer *lexer, bool as_trivia) {
  int depth = 1;
  while (depth > 0 && !lexer->eof(lexer)) {
    if (lexer->lookahead == '*') {
      lexer->advance(lexer, as_trivia);
      if (!lexer->eof(lexer) && lexer->lookahead == '/') {
        lexer->advance(lexer, as_trivia);
        depth--;
      }
      continue;
    }
    if (lexer->lookahead == '/') {
      lexer->advance(lexer, as_trivia);
      if (!lexer->eof(lexer) && lexer->lookahead == '*') {
        lexer->advance(lexer, as_trivia);
        depth++;
      }
      continue;
    }
    lexer->advance(lexer, as_trivia);
  }
  return depth == 0;
}

static bool is_identifier_continue(int32_t c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
         (c >= '0' && c <= '9') || c == '_';
}

static bool skip_quoted_string(TSLexer *lexer) {
  advance(lexer);
  while (!lexer->eof(lexer)) {
    if (lexer->lookahead == '\\') {
      advance(lexer);
      if (lexer->eof(lexer)) return false;
      advance(lexer);
      continue;
    }
    if (lexer->lookahead == '"') {
      advance(lexer);
      return true;
    }
    advance(lexer);
  }
  return false;
}

static bool skip_type_trivia(TSLexer *lexer) {
  for (;;) {
    while (!lexer->eof(lexer) &&
           (lexer->lookahead == ' ' || lexer->lookahead == '\t' ||
            lexer->lookahead == '\r' || lexer->lookahead == '\n')) {
      advance(lexer);
    }
    if (lexer->lookahead != '/') return true;
    advance(lexer);
    if (lexer->lookahead == '/') {
      while (!lexer->eof(lexer) && lexer->lookahead != '\n') {
        advance(lexer);
      }
      continue;
    }
    if (lexer->lookahead != '*') return false;
    advance(lexer);
    if (!consume_block_comment(lexer, false)) return false;
  }
}

static bool scan_qualified_type_after_view(TSLexer *lexer) {
  int angle_depth = 0;
  int paren_depth = 0;
  int bracket_depth = 0;
  int brace_depth = 0;
  bool previous_was_identifier = false;
  bool saw_top_level_as = false;

  while (!lexer->eof(lexer)) {
    int32_t c = lexer->lookahead;
    bool top_level = angle_depth == 1 && paren_depth == 0 &&
                     bracket_depth == 0 && brace_depth == 0;

    if (top_level && c == 'a' && !previous_was_identifier) {
      advance(lexer);
      if (lexer->lookahead == 's') {
        advance(lexer);
        if (!is_identifier_continue(lexer->lookahead)) {
          saw_top_level_as = true;
          previous_was_identifier = true;
          continue;
        }
      }
      previous_was_identifier = is_identifier_continue(lexer->lookahead);
      continue;
    }

    switch (c) {
      case '/':
        advance(lexer);
        if (lexer->lookahead == '/') {
          while (!lexer->eof(lexer) && lexer->lookahead != '\n') {
            advance(lexer);
          }
        } else if (lexer->lookahead == '*') {
          advance(lexer);
          if (!consume_block_comment(lexer, false)) return false;
        }
        previous_was_identifier = false;
        break;
      case '"':
        if (!skip_quoted_string(lexer)) return false;
        previous_was_identifier = false;
        break;
      case '<':
        if (paren_depth == 0 && bracket_depth == 0 && brace_depth == 0) {
          angle_depth++;
        }
        previous_was_identifier = false;
        advance(lexer);
        break;
      case '>':
        if (paren_depth != 0 || bracket_depth != 0 || brace_depth != 0) {
          previous_was_identifier = false;
          advance(lexer);
          break;
        }
        angle_depth--;
        previous_was_identifier = false;
        advance(lexer);
        if (angle_depth == 0) {
          if (!saw_top_level_as || !skip_type_trivia(lexer) ||
              lexer->lookahead != ':')
            return false;
          advance(lexer);
          return lexer->lookahead == ':';
        }
        break;
      case '(':
        paren_depth++;
        previous_was_identifier = false;
        advance(lexer);
        break;
      case ')':
        if (paren_depth == 0) return false;
        paren_depth--;
        previous_was_identifier = false;
        advance(lexer);
        break;
      case '[':
        bracket_depth++;
        previous_was_identifier = false;
        advance(lexer);
        break;
      case ']':
        if (bracket_depth == 0) return false;
        bracket_depth--;
        previous_was_identifier = false;
        advance(lexer);
        break;
      case '{':
        brace_depth++;
        previous_was_identifier = false;
        advance(lexer);
        break;
      case '}':
        if (brace_depth == 0) return false;
        brace_depth--;
        previous_was_identifier = false;
        advance(lexer);
        break;
      default:
        previous_was_identifier = is_identifier_continue(c);
        advance(lexer);
        break;
    }
  }
  return false;
}

static bool scan_view_mode(TSLexer *lexer, bool prefix_is_valid,
                           bool mode_only_is_valid) {
  while (!lexer->eof(lexer) &&
         (lexer->lookahead == ' ' || lexer->lookahead == '\t' ||
          lexer->lookahead == '\r' || lexer->lookahead == '\n')) {
    skip(lexer);
  }

  const char *keyword = "view";
  for (const char *cursor = keyword; *cursor != '\0'; cursor++) {
    if (lexer->lookahead != *cursor) return false;
    advance(lexer);
  }
  if (is_identifier_continue(lexer->lookahead)) return false;
  lexer->mark_end(lexer);

  if (!skip_type_trivia(lexer)) return false;

  if (lexer->lookahead == '<') {
    if (!prefix_is_valid || !scan_qualified_type_after_view(lexer)) return false;
    lexer->result_symbol = VIEW_MODE_PREFIX;
    return true;
  }
  if (lexer->lookahead == ':' || lexer->lookahead == '\0') return false;
  if (mode_only_is_valid &&
      (lexer->lookahead == ',' || lexer->lookahead == '|')) {
    lexer->result_symbol = VIEW_MODE_ONLY;
    return true;
  }
  if (prefix_is_valid &&
      (is_identifier_continue(lexer->lookahead) ||
       lexer->lookahead == '*' || lexer->lookahead == '(' ||
       lexer->lookahead == '[' || lexer->lookahead == '!')) {
    lexer->result_symbol = VIEW_MODE_PREFIX;
    return true;
  }
  return false;
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
        consume_block_comment(lexer, true);
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


bool tree_sitter_fe_external_scanner_scan(void *payload, TSLexer *lexer,
                                          const bool *valid_symbols) {
  (void)payload;

  // Skip if in error recovery mode (all symbols valid)
  if (valid_symbols[AUTOMATIC_SEMICOLON] &&
      valid_symbols[BLOCK_COMMENT_CONTENT] &&
      valid_symbols[BLOCK_COMMENT_END] &&
      valid_symbols[GENERIC_OPEN] &&
      valid_symbols[COMPARISON_LT] &&
      valid_symbols[VIEW_MODE_PREFIX] &&
      valid_symbols[VIEW_MODE_ONLY]) {
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

  if ((valid_symbols[VIEW_MODE_PREFIX] || valid_symbols[VIEW_MODE_ONLY]) &&
      scan_view_mode(lexer, valid_symbols[VIEW_MODE_PREFIX],
                     valid_symbols[VIEW_MODE_ONLY])) {
    return true;
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
        // If only GENERIC_OPEN is valid (no COMPARISON_LT), this is a generic
        // context (e.g., start of qualified path <<T as Trait>::Item as ...>).
        // Emit just the first '<' as GENERIC_OPEN.
        if (valid_symbols[GENERIC_OPEN] && !valid_symbols[COMPARISON_LT]) {
          lexer->mark_end(lexer);  // mark end after first '<'
          lexer->result_symbol = GENERIC_OPEN;
          return true;
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
                if (angle_depth == 0) { is_generic = true; goto done_scanning; }
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
