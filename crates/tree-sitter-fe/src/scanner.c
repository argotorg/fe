#include "tree_sitter/parser.h"
#include <stdbool.h>

// Must match the order in grammar.js externals array
enum TokenType {
  AUTOMATIC_SEMICOLON,
  BLOCK_COMMENT_CONTENT,
  BLOCK_COMMENT_END,
  GENERIC_OPEN,
  COMPARISON_LT,
};

void *tree_sitter_fe_external_scanner_create(void) { return NULL; }
void tree_sitter_fe_external_scanner_destroy(void *payload) {}
unsigned tree_sitter_fe_external_scanner_serialize(void *payload, char *buffer) { return 0; }
void tree_sitter_fe_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {}

static void advance(TSLexer *lexer) { lexer->advance(lexer, false); }
static void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

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
// non-whitespace token on the *next* line. If it is a continuation token
// (currently just `.` for method chaining), we suppress the semicolon so
// that multi-line expressions work correctly:
//
//   x
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
      // Just `/` -- division operator on the same line (no newline yet)
      // or continuation after newline
      if (saw_newline) return false; // `/` is not a continuation token
      return false;
    }

    // We've found a non-whitespace, non-comment character.
    if (!saw_newline) {
      // Still on the same line -- no semicolon (more expression follows)
      return false;
    }

    // We're past a newline. Check for continuation tokens.
    // `.` means method chaining -- don't insert semicolon
    if (c == '.') return false;

    // Any other token after a newline -- insert semicolon
    return true;
  }
}


bool tree_sitter_fe_external_scanner_scan(void *payload, TSLexer *lexer,
                                          const bool *valid_symbols) {
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
        int chars_scanned = 0;
        const int MAX_LOOKAHEAD = 256;
        bool is_generic = false;
        // Track last non-whitespace char at top level (all depths 0) to
        // distinguish `Foo<T, {expr}>` ('{' after ',') from `if x < y {` ('{' after identifier).
        int32_t last_top_char = '<';  // Starts as '<' since we just consumed it.

        while (!lexer->eof(lexer) && chars_scanned < MAX_LOOKAHEAD) {
          int32_t c = lexer->lookahead;
          chars_scanned++;

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
