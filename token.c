#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <error.h>

#include "token.h"

struct Token* new_identifier(int kind, char *lexeme) {
  struct Token *token = alloc_token();
  
  if (NULL == lexeme) {
    error(0, 0, "identifier requires lexeme");
    exit(1);
  }

  token->kind = kind;
  token->number = 0;
  
  size_t lexeme_len = strlen(lexeme) + 1;
  if (lexeme_len > MAX_LEXEME_LEN) {
    error(0, 0, "lexeme too long");
    exit(1);
  }
  char *copied_lexeme = malloc(sizeof(char) * lexeme_len);
  if (NULL == copied_lexeme) {
    perror("malloc");
    exit(1);
  }
  strncpy(copied_lexeme, lexeme, lexeme_len);
  token->identifier = copied_lexeme;
  /* printf("identifier %s\n", token->identifier); */
  return token;
}

struct Token* new_number(int kind, int number) {
  struct Token *token = alloc_token();
  token->kind = kind;
  token->identifier = NULL;
  token->number = number;
  /* printf("number %d\n", number); */
  return token;
}

struct Token* new_token(int kind) {
  struct Token *token = alloc_token();
  token->kind = kind;
  token->identifier = NULL;
  token->number = 0;
  return token;
}

struct Token *alloc_token() {
  struct Token *token = malloc(sizeof(struct Token));
  if (NULL == token) {
    perror("malloc");
    exit(1);
  }
  return token;
}

struct Token** alloc_token_list() {
  struct Token** token_list = malloc(sizeof(struct Token*)*MAX_TOKENS);
  if (NULL == token_list) {
    perror("malloc");
    exit(1);
  }
  for (int i = 0; i < MAX_TOKENS; i++) {
    token_list[i] = 0;
  }
  return token_list;
}

void print_token_kind(FILE *fp, enum tok_kind kind) {
  switch (kind) {
  case VAR:
    fprintf(fp, "VAR");
    break;

  case FUNC:
    fprintf(fp, "FUNC");
    break;

  case INT:
    fprintf(fp, "INT");
    break;

  case BOOL:
    fprintf(fp, "BOOL");
    break;

  case RETURN:
    fprintf(fp, "RETURN");
    break;

  case BEGIN:
    fprintf(fp, "BEGIN");
    break;

  case END:
    fprintf(fp, "END");
    break;

  case IF:
    fprintf(fp, "IF");
    break;

  case THEN:
    fprintf(fp, "THEN");
    break;

  case ELSE:
    fprintf(fp, "ELSE");
    break;

  case WHILE:
    fprintf(fp, "WHILE");
    break;

  case DO:
    fprintf(fp, "DO");
    break;

  case READ:
    fprintf(fp, "READ");
    break;

  case WRITE:
    fprintf(fp, "WRITE");
    break;

  case TRUE:
    fprintf(fp, "TRUE");
    break;

  case FALSE:
    fprintf(fp, "FALSE");
    break;

  case OR:
    fprintf(fp, "OR");
    break;

  case AND:
    fprintf(fp, "AND");
    break;

  case NOT:
    fprintf(fp, "NOT");
    break;

  case EQ:
    fprintf(fp, "EQ");
    break;

  case COMMA:
    fprintf(fp, "COMMA");
    break;

  case COLON:
    fprintf(fp, "COLON");
    break;

  case ASSIGN:
    fprintf(fp, "ASSIGN");
    break;

  case PLUS:
    fprintf(fp, "PLUS");
    break;

  case MINUS:
    fprintf(fp, "MINUS");
    break;

  case MULT:
    fprintf(fp, "MULT");
    break;

  case DIV:
    fprintf(fp, "DIV");
    break;

  case MOD:
    fprintf(fp, "MOD");
    break;

  case LPAREN:
    fprintf(fp, "LPAREN");
    break;

  case RPAREN:
    fprintf(fp, "RPAREN");
    break;

  case LT:
    fprintf(fp, "LT");
    break;

  case LTE:
    fprintf(fp, "LTE");
    break;

  case GT:
    fprintf(fp, "GT");
    break;

  case GTE:
    fprintf(fp, "GTE");
    break;

  case NEQ:
    fprintf(fp, "NEQ");
    break;

  case IDENT:
    fprintf(fp, "IDENT");
    break;

  case NUMBER:
    fprintf(fp, "NUMBER");
    break;

  case EOT:
    fprintf(fp, "EOT");
    break;

  default:
    error(0, 0, "unknown token kind");
    exit(1);
  }
}

void print_token(FILE *fp, struct Token *token) {
  if (NULL != token) {
    print_token_kind(fp, token->kind);
    switch (token->kind) {
    case IDENT:
      fprintf(fp, " %s", token->identifier);
      break;

    case NUMBER:
      fprintf(fp, " %d", token->number);
      break;
    }
  }
}

void write_tokens(FILE *fp, struct Token **list) {
  fflush(stdout);
  int count = 0;
  while (true) {
    if (count > MAX_TOKENS) {
      error(0, 0, "exceeded maximum tokens");
      exit(1);
    }

    struct Token *token = list[count++];

    if (NULL == token) break;

    print_token(fp, token);

    fprintf(fp, "\n");
  }
}
