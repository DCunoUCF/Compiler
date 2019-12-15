#ifndef TOKEN_H
#define TOKEN_H

#include <stdio.h>

#define MAX_LEXEME_LEN 512

#define MAX_TOKENS 4096

typedef enum tok_kind Type;

enum tok_kind {
  VAR = 1,
  FUNC,
  INT,
  BOOL,
  RETURN,
  BEGIN,
  END,
  IF,
  THEN,
  ELSE,
  WHILE,
  DO,
  READ,
  WRITE,
  TRUE,
  FALSE,
  OR,
  AND,
  NOT,
  EQ,
  COMMA,
  COLON,
  ASSIGN,
  PLUS,
  MINUS,
  MULT,
  DIV,
  MOD,
  LPAREN,
  RPAREN,
  LT,
  LTE,
  GT,
  GTE,
  NEQ,
  IDENT,
  NUMBER,
  EOT,
};

struct Token {
  enum tok_kind kind;
  union {
    char *identifier;  // only when kind == IDENT
    int number;  // only when kind == NUMBER
  };
};

struct Token *alloc_token();

struct Token** alloc_token_list();

struct Token* new_identifier(int kind, char *lexeme);

struct Token* new_number(int kind, int number);

struct Token* new_token(int kind);

struct Token *alloc_token();

struct Token** alloc_token_list();

void write_tokens(FILE *, struct Token**);

void print_token_kind(FILE *, enum tok_kind);

void print_token(FILE *, struct Token*);

struct Token** read_tokens(FILE *);

#endif
