#include <stdio.h>
#include <stdbool.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <error.h>

#include "token.h"
#include "lexer.h"

#include "lexer_reserved.c"

struct Token** lexer(FILE *lexerin) {
  int token_list_i = 0;
  struct Token** token_list = alloc_token_list();
  char c;
  
  c = fgetc(lexerin);
  while (true) {
    char buffer[MAX_LEXEME_LEN];
    int buffer_i = 0;

    if (token_list_i > MAX_TOKENS) {
      error(0, 0, "exceeded maximum tokens");
      exit(1);
    }

    if (isalpha(c) || '_' == c) {
      // buffer an identifier
      buffer[buffer_i++] = c;
      c = fgetc(lexerin);
      while((isalpha(c) || '_' == c || isdigit(c)) && buffer_i + 1 < MAX_LEXEME_LEN) {
        buffer[buffer_i++] = c;
        c = fgetc(lexerin);
      }
      // adding '\0' sentinel
      buffer[buffer_i] = '\0';

      // hash function to match reserved words is given
      struct reserved_type *reserved = lexer_reserved(buffer, strlen(buffer));
      if (NULL != reserved) {
        // reserved word
        token_list[token_list_i++] = new_token(reserved->type);
      } else {
        // create token for identifier with its lexeme
        token_list[token_list_i++] = new_identifier(IDENT, buffer);
      }
    } else if (isdigit(c)) {
      buffer[buffer_i++] = c;
      c = fgetc(lexerin);
      while(isdigit(c) && buffer_i + 1 < MAX_LEXEME_LEN) {
        buffer[buffer_i++] = c;
        c = fgetc(lexerin);
      }
      // adding '\0' sentinel
      buffer[buffer_i] = '\0';

      token_list[token_list_i++] = new_number(NUMBER, atoi(buffer));
    } else if (ispunct(c)) {
      if ('#' == c) { // given
        while ('\n' !=  (c = fgetc(lexerin)));
      } else if ('=' == c) { // given
        token_list[token_list_i++] = new_token(EQ);
        c = fgetc(lexerin);
      } else if (',' == c) { // given
        token_list[token_list_i++] = new_token(COMMA);
        c = fgetc(lexerin);
      } else if (':' == c) {
        c = fgetc(lexerin);
        if('=' == c) {
          token_list[token_list_i++] = new_token(ASSIGN);
          c = fgetc(lexerin);
        } else {
          token_list[token_list_i++] = new_token(COLON);
        }
      } else if ('+' == c) {
        token_list[token_list_i++] = new_token(PLUS);
        c = fgetc(lexerin);
      } else if ('-' == c) {
        token_list[token_list_i++] = new_token(MINUS);
        c = fgetc(lexerin);
      } else if ('*' == c) {
        token_list[token_list_i++] = new_token(MULT);
        c = fgetc(lexerin);
      } else if ('/' == c) {
        token_list[token_list_i++] = new_token(DIV);
        c = fgetc(lexerin);
      } else if ('%' == c) {
        token_list[token_list_i++] = new_token(MOD);
        c = fgetc(lexerin);
      } else if ('(' == c) {
        token_list[token_list_i++] = new_token(LPAREN);
        c = fgetc(lexerin);
      } else if (')' == c) {
        token_list[token_list_i++] = new_token(RPAREN);
        c = fgetc(lexerin);
      } else if ('<' == c) {
        c = fgetc(lexerin);
        if('=' == c) {
          token_list[token_list_i++] = new_token(LTE);
          c = fgetc(lexerin);
        } else if('>' == c){
          token_list[token_list_i++] = new_token(NEQ);
          c = fgetc(lexerin);
        } else {
          token_list[token_list_i++] = new_token(LT);
      }} else if ('>' == c) {
        c = fgetc(lexerin);
        if('=' == c) {
          token_list[token_list_i++] = new_token(GTE);
          c = fgetc(lexerin);
        } else {
          token_list[token_list_i++] = new_token(GT);
        }
      } else {
        error(0, 0, "invalid token");
        exit(1);
      }
    } else if (isspace(c)) {
      c = fgetc(lexerin);
    } else if (c == EOF) {
      token_list[token_list_i++] = new_token(EOT);
      break;
    }

    // clearing buffer with '\0' sentinels
    int i_count;
    for(i_count = 0; i_count <= buffer_i; i_count++){
      buffer[i_count] = '\0';
    }
  }

  return token_list;
}
