
#ifndef AST_H
#define AST_H

#include <stdio.h>

#include "token.h"

struct TranslationUnit;
struct Block;
struct FuncDecl;
struct TypedIdent;
struct TypedIdentList;
struct FuncDeclList;
enum statement_union;
struct Statement;
struct StatementList;
enum expression_union;
struct Expression;
struct ExpressionList;

#include "datatype.h"

struct TranslationUnit *newTranslationUnit();
struct Block *newBlock();
struct FuncDecl *newFuncDecl();
struct TypedIdent *newTypedIdent();
struct TypedIdentList *newTypedIdentList();
struct FuncDeclList *newFuncDeclList();
struct Statement *newStatement(enum statement_union kind);
struct StatementList *newStatementList();
struct Expression *newExpression(enum expression_union kind);
struct ExpressionList *newExpressionList();

void addTypedIdent(struct TypedIdentList *list, struct TypedIdent *node);
void addFuncDecl(struct FuncDeclList *list, struct FuncDecl *node);
void addStatement(struct StatementList *list, struct Statement *node);
void addExpression(struct ExpressionList *list, struct Expression *node);

void printIndent(FILE *fp, int depth);

void printstring(FILE *fp, char *string, int depth);
void printbool(FILE *fp, bool value, int depth);
void printint(FILE *fp, int value, int depth);
void printToken(FILE *fp, struct Token *token, int depth);
void printstringTyped(FILE *fp, char *string, int depth);
void printboolTyped(FILE *fp, bool value, int depth);
void printintTyped(FILE *fp, int value, int depth);
void printTokenTyped(FILE *fp, struct Token *token, int depth);

void printTranslationUnit(FILE *, struct TranslationUnit *, int );
void printBlock(FILE *, struct Block *, int );
void printFuncDecl(FILE *, struct FuncDecl *, int );
void printTypedIdent(FILE *, struct TypedIdent *, int );
void printTypedIdentList(FILE *, struct TypedIdentList *, int );
void printFuncDeclList(FILE *, struct FuncDeclList *, int );
void printStatement(FILE *, struct Statement *, int );
void printStatementList(FILE *, struct StatementList *, int );
void printExpression(FILE *, struct Expression *, int );
void printExpressionList(FILE *, struct ExpressionList *, int );

void printTranslationUnitTyped(FILE *, struct TranslationUnit *, int );
void printBlockTyped(FILE *, struct Block *, int );
void printFuncDeclTyped(FILE *, struct FuncDecl *, int );
void printTypedIdentTyped(FILE *, struct TypedIdent *, int );
void printTypedIdentListTyped(FILE *, struct TypedIdentList *, int );
void printFuncDeclListTyped(FILE *, struct FuncDeclList *, int );
void printStatementTyped(FILE *, struct Statement *, int );
void printStatementListTyped(FILE *, struct StatementList *, int );
void printExpressionTyped(FILE *, struct Expression *, int );
void printExpressionListTyped(FILE *, struct ExpressionList *, int );

struct TranslationUnit {
  struct Scope *scope;
  struct Block *block;
};

struct Block {
  struct TypedIdentList *vardecls;
  struct FuncDeclList *funcdecls;
  struct Statement *statement;
};

struct FuncDecl {
  struct Scope *scope;
  struct Symbol *symbol;
  char *identifier;
  struct TypedIdentList *formals;
  bool has_return;
  struct Token *return_type;
  struct Block *block;
};

struct TypedIdent {
  struct Symbol *symbol;
  char *identifier;
  struct Token *type;
};

struct TypedIdentList {
  struct TypedIdentListElement *head;
  struct TypedIdentListElement *tail;
  int size;
};

struct TypedIdentListElement {
  struct TypedIdent *node;
  struct TypedIdentListElement *next;
};

struct FuncDeclList {
  struct FuncDeclListElement *head;
  struct FuncDeclListElement *tail;
  int size;
};

struct FuncDeclListElement {
  struct FuncDecl *node;
  struct FuncDeclListElement *next;
};

enum statement_union {
  ASSIGNSTATEMENT,
  CALLSTATEMENT,
  RETURNSTATEMENT,
  COMPOUNDSTATEMENT,
  IFSTATEMENT,
  WHILESTATEMENT,
  READSTATEMENT,
  WRITESTATEMENT,
};

struct Statement {
  enum statement_union kind;
  union {
    struct { //  ASSIGNSTATEMENT
      struct Symbol *assign_symbol;
      char *assign_variable;
      struct Expression *assign_expression;
    };
    struct { //  CALLSTATEMENT
      struct Symbol *call_symbol;
      char *call_function;
      struct ExpressionList *call_parameters;
    };
    struct { //  RETURNSTATEMENT
      struct Symbol *function_symbol;
      struct Expression *return_expression;
    };
    struct { //  COMPOUNDSTATEMENT
      struct StatementList *compound_statement;
    };
    struct { //  IFSTATEMENT
      struct Expression *if_condition;
      struct Statement *if_branch;
      struct Statement *if_elsebranch;
    };
    struct { //  WHILESTATEMENT
      struct Expression *while_condition;
      struct Statement *while_body;
    };
    struct { //  READSTATEMENT
      struct Symbol *read_symbol;
      char *read_variable;
    };
    struct { //  WRITESTATEMENT
      struct Expression *write_expression;
    };
  };
};

struct StatementList {
  struct StatementListElement *head;
  struct StatementListElement *tail;
  int size;
};

struct StatementListElement {
  struct Statement *node;
  struct StatementListElement *next;
};

enum expression_union {
  BINARYEXPRESSION,
  UNARYEXPRESSION,
  NUMBERFACTOR,
  BOOLEANFACTOR,
  VARIABLEFACTOR,
  FUNCTIONFACTOR,
};

struct Expression {
  int  ershov;
  struct DataType * datatype;
  enum expression_union kind;
  union {
    struct { //  BINARYEXPRESSION
      struct Token *binary_op;
      struct Expression *binary_left;
      struct Expression *binary_right;
    };
    struct { //  UNARYEXPRESSION
      struct Token *unary_op;
      struct Expression *unary_expression;
    };
    struct { //  NUMBERFACTOR
      int number_value;
    };
    struct { //  BOOLEANFACTOR
      bool boolean_value;
    };
    struct { //  VARIABLEFACTOR
      struct Symbol *variable_symbol;
      char *variable;
    };
    struct { //  FUNCTIONFACTOR
      struct Symbol *function_symbol;
      char *function_name;
      struct ExpressionList *function_parameters;
    };
  };
};

struct ExpressionList {
  struct ExpressionListElement *head;
  struct ExpressionListElement *tail;
  int size;
};

struct ExpressionListElement {
  struct Expression *node;
  struct ExpressionListElement *next;
};

#endif
