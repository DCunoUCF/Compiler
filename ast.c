
#include <stdbool.h>
#include <malloc.h>
#include <stdlib.h>
#include <error.h>

#include "token.h"
#include "datatype.h"
#include "ast.h"
#include "symtab.h"

void printIndent(FILE *fp, int depth) {
  if (depth > 0) {
    for (int i = 0; i < depth; i++) {
      fprintf(fp, "| ");
    }
  }
}

void printstring(FILE *fp, char *string, int depth) {
  fprintf(fp, "%s", string);
}

void printbool(FILE *fp, bool value, int depth) {
  if (value) {
    fprintf(fp, "true");
  } else {
    fprintf(fp, "false");
  }
}

void printint(FILE *fp, int value, int depth) {
  fprintf(fp, "%d", value);
}

void printToken(FILE *fp, struct Token *token, int depth) {
  print_token(fp, token);
}

void printDataType(FILE *fp, struct DataType *datatype, int depth) {
  if (NULL != datatype) {
    print_data_type(fp, datatype);
  }
}

void printScope(FILE *fp, struct Scope *scope, int depth) {
  if (NULL != scope) {
    fprintf(fp, "%s", scope->name);
  }
}

void printSymbol(FILE *fp, struct Symbol *symbol, int depth) {
  if (NULL != symbol) {
    fprintf(fp, "%s.%s", symbol->scope->name, symbol->name);
  }
}

void printstringTyped(FILE *fp, char *string, int depth) {
  printstring(fp, string, depth);
}

void printboolTyped(FILE *fp, bool value, int depth) {
  printbool(fp, value, depth);
}

void printintTyped(FILE *fp, int value, int depth) {
  printint(fp, value, depth);
}

void printTokenTyped(FILE *fp, struct Token *token, int depth) {
  printToken(fp, token, depth);
}

void printDataTypeTyped(FILE *fp, struct DataType *datatype, int depth) {
  printDataType(fp, datatype, depth);
}

void printScopeTyped(FILE *fp, struct Scope *scope, int depth) {
  printScope(fp, scope, depth);
}

void printSymbolTyped(FILE *fp, struct Symbol *symbol, int depth) {
  printSymbol(fp, symbol, depth);
}

struct TranslationUnit *newTranslationUnit() {
  struct TranslationUnit *node = malloc(sizeof(struct TranslationUnit));
  if (NULL == node) {
    perror("malloc");
    exit(1);
  }
  return node;
}

struct Block *newBlock() {
  struct Block *node = malloc(sizeof(struct Block));
  if (NULL == node) {
    perror("malloc");
    exit(1);
  }
  return node;
}

struct FuncDecl *newFuncDecl() {
  struct FuncDecl *node = malloc(sizeof(struct FuncDecl));
  if (NULL == node) {
    perror("malloc");
    exit(1);
  }
  return node;
}

struct TypedIdent *newTypedIdent() {
  struct TypedIdent *node = malloc(sizeof(struct TypedIdent));
  if (NULL == node) {
    perror("malloc");
    exit(1);
  }
  return node;
}

struct TypedIdentList *newTypedIdentList() {
  struct TypedIdentList *list = malloc(sizeof(struct TypedIdentList));
  if (NULL == list) {
    perror("malloc");
    exit(1);
  }
  list->head = NULL;
  list->tail = NULL;
  list->size = 0;
  return list;
}

struct TypedIdentListElement *newTypedIdentListElement() {
  struct TypedIdentListElement *elem = malloc(sizeof(struct TypedIdentListElement));
  if (NULL == elem) {
    perror("malloc");
    exit(1);
  }
  elem->node = NULL;
  elem->next = NULL;
  return elem;
}

void addTypedIdent(struct TypedIdentList *list, struct TypedIdent *node) {
  struct TypedIdentListElement *elem = newTypedIdentListElement();
  elem->node = node;
  elem->next = NULL;
  if (NULL == list->head) {
    list->head = list->tail = elem;
    list->size = 1;
  } else {
    list->tail->next = elem;
    list->tail = list->tail->next;
    list->size++;
  }
}

struct FuncDeclList *newFuncDeclList() {
  struct FuncDeclList *list = malloc(sizeof(struct FuncDeclList));
  if (NULL == list) {
    perror("malloc");
    exit(1);
  }
  list->head = NULL;
  list->tail = NULL;
  list->size = 0;
  return list;
}

struct FuncDeclListElement *newFuncDeclListElement() {
  struct FuncDeclListElement *elem = malloc(sizeof(struct FuncDeclListElement));
  if (NULL == elem) {
    perror("malloc");
    exit(1);
  }
  elem->node = NULL;
  elem->next = NULL;
  return elem;
}

void addFuncDecl(struct FuncDeclList *list, struct FuncDecl *node) {
  struct FuncDeclListElement *elem = newFuncDeclListElement();
  elem->node = node;
  elem->next = NULL;
  if (NULL == list->head) {
    list->head = list->tail = elem;
    list->size = 1;
  } else {
    list->tail->next = elem;
    list->tail = list->tail->next;
    list->size++;
  }
}

struct Statement *newStatement(enum statement_union kind) {
  struct Statement *node = malloc(sizeof(struct Statement));
  if (NULL == node) {
    perror("malloc");
    exit(1);
  }
  node->kind = kind;

  return node;
}

struct StatementList *newStatementList() {
  struct StatementList *list = malloc(sizeof(struct StatementList));
  if (NULL == list) {
    perror("malloc");
    exit(1);
  }
  list->head = NULL;
  list->tail = NULL;
  list->size = 0;
  return list;
}

struct StatementListElement *newStatementListElement() {
  struct StatementListElement *elem = malloc(sizeof(struct StatementListElement));
  if (NULL == elem) {
    perror("malloc");
    exit(1);
  }
  elem->node = NULL;
  elem->next = NULL;
  return elem;
}

void addStatement(struct StatementList *list, struct Statement *node) {
  struct StatementListElement *elem = newStatementListElement();
  elem->node = node;
  elem->next = NULL;
  if (NULL == list->head) {
    list->head = list->tail = elem;
    list->size = 1;
  } else {
    list->tail->next = elem;
    list->tail = list->tail->next;
    list->size++;
  }
}

struct Expression *newExpression(enum expression_union kind) {
  struct Expression *node = malloc(sizeof(struct Expression));
  if (NULL == node) {
    perror("malloc");
    exit(1);
  }
  node->kind = kind;

  return node;
}

struct ExpressionList *newExpressionList() {
  struct ExpressionList *list = malloc(sizeof(struct ExpressionList));
  if (NULL == list) {
    perror("malloc");
    exit(1);
  }
  list->head = NULL;
  list->tail = NULL;
  list->size = 0;
  return list;
}

struct ExpressionListElement *newExpressionListElement() {
  struct ExpressionListElement *elem = malloc(sizeof(struct ExpressionListElement));
  if (NULL == elem) {
    perror("malloc");
    exit(1);
  }
  elem->node = NULL;
  elem->next = NULL;
  return elem;
}

void addExpression(struct ExpressionList *list, struct Expression *node) {
  struct ExpressionListElement *elem = newExpressionListElement();
  elem->node = node;
  elem->next = NULL;
  if (NULL == list->head) {
    list->head = list->tail = elem;
    list->size = 1;
  } else {
    list->tail->next = elem;
    list->tail = list->tail->next;
    list->size++;
  }
}


void printTranslationUnit(FILE *fp, struct TranslationUnit *node, int depth) {
  if (NULL == node) return;
  fprintf(fp, "TranslationUnit");
  depth++;
  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "block = ");
  printBlock(fp, node->block, depth);

}

void printTranslationUnitTyped(FILE *fp, struct TranslationUnit *node, int depth) {
  if (NULL == node) return;
  fprintf(fp, "TranslationUnit");
  depth++;
  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "scope = ");
  printScopeTyped(fp, node->scope, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "block = ");
  printBlockTyped(fp, node->block, depth);

}

void printBlock(FILE *fp, struct Block *node, int depth) {
  if (NULL == node) return;
  fprintf(fp, "Block");
  depth++;
  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "vardecls = ");
  printTypedIdentList(fp, node->vardecls, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "funcdecls = ");
  printFuncDeclList(fp, node->funcdecls, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "statement = ");
  printStatement(fp, node->statement, depth);

}

void printBlockTyped(FILE *fp, struct Block *node, int depth) {
  if (NULL == node) return;
  fprintf(fp, "Block");
  depth++;
  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "vardecls = ");
  printTypedIdentListTyped(fp, node->vardecls, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "funcdecls = ");
  printFuncDeclListTyped(fp, node->funcdecls, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "statement = ");
  printStatementTyped(fp, node->statement, depth);

}

void printFuncDecl(FILE *fp, struct FuncDecl *node, int depth) {
  if (NULL == node) return;
  fprintf(fp, "FuncDecl");
  depth++;
  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "identifier = ");
  printstring(fp, node->identifier, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "formals = ");
  printTypedIdentList(fp, node->formals, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "has_return = ");
  printbool(fp, node->has_return, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "return_type = ");
  printToken(fp, node->return_type, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "block = ");
  printBlock(fp, node->block, depth);

}

void printFuncDeclTyped(FILE *fp, struct FuncDecl *node, int depth) {
  if (NULL == node) return;
  fprintf(fp, "FuncDecl");
  depth++;
  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "scope = ");
  printScopeTyped(fp, node->scope, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "symbol = ");
  printSymbolTyped(fp, node->symbol, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "identifier = ");
  printstringTyped(fp, node->identifier, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "formals = ");
  printTypedIdentListTyped(fp, node->formals, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "has_return = ");
  printboolTyped(fp, node->has_return, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "return_type = ");
  printTokenTyped(fp, node->return_type, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "block = ");
  printBlockTyped(fp, node->block, depth);

}

void printTypedIdent(FILE *fp, struct TypedIdent *node, int depth) {
  if (NULL == node) return;
  fprintf(fp, "TypedIdent");
  depth++;
  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "identifier = ");
  printstring(fp, node->identifier, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "type = ");
  printToken(fp, node->type, depth);

}

void printTypedIdentTyped(FILE *fp, struct TypedIdent *node, int depth) {
  if (NULL == node) return;
  fprintf(fp, "TypedIdent");
  depth++;
  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "symbol = ");
  printSymbolTyped(fp, node->symbol, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "identifier = ");
  printstringTyped(fp, node->identifier, depth);

  fprintf(fp, "\n");
  printIndent(fp, depth);
  fprintf(fp, "type = ");
  printTokenTyped(fp, node->type, depth);

}

void printTypedIdentList(FILE *fp, struct TypedIdentList *list, int depth) {
  if (NULL == list) return;
  fprintf(fp, "TypedIdentList");
  depth++;
  struct TypedIdentListElement *cur = list->head;
  while (NULL != cur) {
    fprintf(fp, "\n");
    printIndent(fp, depth);
    printTypedIdent(fp, cur->node, depth);
    cur = cur->next;
  }
}

void printTypedIdentListTyped(FILE *fp, struct TypedIdentList *list, int depth) {
  if (NULL == list) return;
  fprintf(fp, "TypedIdentList");
  depth++;
  struct TypedIdentListElement *cur = list->head;
  while (NULL != cur) {
    fprintf(fp, "\n");
    printIndent(fp, depth);
    printTypedIdentTyped(fp, cur->node, depth);
    cur = cur->next;
  }
}

void printFuncDeclList(FILE *fp, struct FuncDeclList *list, int depth) {
  if (NULL == list) return;
  fprintf(fp, "FuncDeclList");
  depth++;
  struct FuncDeclListElement *cur = list->head;
  while (NULL != cur) {
    fprintf(fp, "\n");
    printIndent(fp, depth);
    printFuncDecl(fp, cur->node, depth);
    cur = cur->next;
  }
}

void printFuncDeclListTyped(FILE *fp, struct FuncDeclList *list, int depth) {
  if (NULL == list) return;
  fprintf(fp, "FuncDeclList");
  depth++;
  struct FuncDeclListElement *cur = list->head;
  while (NULL != cur) {
    fprintf(fp, "\n");
    printIndent(fp, depth);
    printFuncDeclTyped(fp, cur->node, depth);
    cur = cur->next;
  }
}

void printStatement(FILE *fp, struct Statement *node, int depth) {
  if (NULL == node) return;
  switch (node->kind) {
  case ASSIGNSTATEMENT:
    fprintf(fp, "AssignStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "assign_variable = ");
    printstring(fp, node->assign_variable, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "assign_expression = ");
    printExpression(fp, node->assign_expression, depth);
    break;
  case CALLSTATEMENT:
    fprintf(fp, "CallStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "call_function = ");
    printstring(fp, node->call_function, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "call_parameters = ");
    printExpressionList(fp, node->call_parameters, depth);
    break;
  case RETURNSTATEMENT:
    fprintf(fp, "ReturnStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "return_expression = ");
    printExpression(fp, node->return_expression, depth);
    break;
  case COMPOUNDSTATEMENT:
    fprintf(fp, "CompoundStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "compound_statement = ");
    printStatementList(fp, node->compound_statement, depth);
    break;
  case IFSTATEMENT:
    fprintf(fp, "IfStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "if_condition = ");
    printExpression(fp, node->if_condition, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "if_branch = ");
    printStatement(fp, node->if_branch, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "if_elsebranch = ");
    printStatement(fp, node->if_elsebranch, depth);
    break;
  case WHILESTATEMENT:
    fprintf(fp, "WhileStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "while_condition = ");
    printExpression(fp, node->while_condition, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "while_body = ");
    printStatement(fp, node->while_body, depth);
    break;
  case READSTATEMENT:
    fprintf(fp, "ReadStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "read_variable = ");
    printstring(fp, node->read_variable, depth);
    break;
  case WRITESTATEMENT:
    fprintf(fp, "WriteStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "write_expression = ");
    printExpression(fp, node->write_expression, depth);
    break;
  default:
    error(0, 0, "unknown option");
    exit(1);
    break;
  }
}

void printStatementTyped(FILE *fp, struct Statement *node, int depth) {
  if (NULL == node) return;
  switch (node->kind) {
  case ASSIGNSTATEMENT:
    fprintf(fp, "AssignStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "assign_symbol = ");
    printSymbolTyped(fp, node->assign_symbol, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "assign_variable = ");
    printstringTyped(fp, node->assign_variable, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "assign_expression = ");
    printExpressionTyped(fp, node->assign_expression, depth);
    break;
  case CALLSTATEMENT:
    fprintf(fp, "CallStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "call_symbol = ");
    printSymbolTyped(fp, node->call_symbol, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "call_function = ");
    printstringTyped(fp, node->call_function, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "call_parameters = ");
    printExpressionListTyped(fp, node->call_parameters, depth);
    break;
  case RETURNSTATEMENT:
    fprintf(fp, "ReturnStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "function_symbol = ");
    printSymbolTyped(fp, node->function_symbol, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "return_expression = ");
    printExpressionTyped(fp, node->return_expression, depth);
    break;
  case COMPOUNDSTATEMENT:
    fprintf(fp, "CompoundStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "compound_statement = ");
    printStatementListTyped(fp, node->compound_statement, depth);
    break;
  case IFSTATEMENT:
    fprintf(fp, "IfStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "if_condition = ");
    printExpressionTyped(fp, node->if_condition, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "if_branch = ");
    printStatementTyped(fp, node->if_branch, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "if_elsebranch = ");
    printStatementTyped(fp, node->if_elsebranch, depth);
    break;
  case WHILESTATEMENT:
    fprintf(fp, "WhileStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "while_condition = ");
    printExpressionTyped(fp, node->while_condition, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "while_body = ");
    printStatementTyped(fp, node->while_body, depth);
    break;
  case READSTATEMENT:
    fprintf(fp, "ReadStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "read_symbol = ");
    printSymbolTyped(fp, node->read_symbol, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "read_variable = ");
    printstringTyped(fp, node->read_variable, depth);
    break;
  case WRITESTATEMENT:
    fprintf(fp, "WriteStatement");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "write_expression = ");
    printExpressionTyped(fp, node->write_expression, depth);
    break;
  default:
    error(0, 0, "unknown option");
    exit(1);
    break;
  }
}

void printStatementList(FILE *fp, struct StatementList *list, int depth) {
  if (NULL == list) return;
  fprintf(fp, "StatementList");
  depth++;
  struct StatementListElement *cur = list->head;
  while (NULL != cur) {
    fprintf(fp, "\n");
    printIndent(fp, depth);
    printStatement(fp, cur->node, depth);
    cur = cur->next;
  }
}

void printStatementListTyped(FILE *fp, struct StatementList *list, int depth) {
  if (NULL == list) return;
  fprintf(fp, "StatementList");
  depth++;
  struct StatementListElement *cur = list->head;
  while (NULL != cur) {
    fprintf(fp, "\n");
    printIndent(fp, depth);
    printStatementTyped(fp, cur->node, depth);
    cur = cur->next;
  }
}

void printExpression(FILE *fp, struct Expression *node, int depth) {
  if (NULL == node) return;
  switch (node->kind) {
  case BINARYEXPRESSION:
    fprintf(fp, "BinaryExpression");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "binary_op = ");
    printToken(fp, node->binary_op, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "binary_left = ");
    printExpression(fp, node->binary_left, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "binary_right = ");
    printExpression(fp, node->binary_right, depth);
    break;
  case UNARYEXPRESSION:
    fprintf(fp, "UnaryExpression");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "unary_op = ");
    printToken(fp, node->unary_op, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "unary_expression = ");
    printExpression(fp, node->unary_expression, depth);
    break;
  case NUMBERFACTOR:
    fprintf(fp, "NumberFactor");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "number_value = ");
    printint(fp, node->number_value, depth);
    break;
  case BOOLEANFACTOR:
    fprintf(fp, "BooleanFactor");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "boolean_value = ");
    printbool(fp, node->boolean_value, depth);
    break;
  case VARIABLEFACTOR:
    fprintf(fp, "VariableFactor");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "variable = ");
    printstring(fp, node->variable, depth);
    break;
  case FUNCTIONFACTOR:
    fprintf(fp, "FunctionFactor");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "function_name = ");
    printstring(fp, node->function_name, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "function_parameters = ");
    printExpressionList(fp, node->function_parameters, depth);
    break;
  default:
    error(0, 0, "unknown option");
    exit(1);
    break;
  }
}

void printExpressionTyped(FILE *fp, struct Expression *node, int depth) {
  if (NULL == node) return;
  switch (node->kind) {
  case BINARYEXPRESSION:
    fprintf(fp, "BinaryExpression");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "ershov = ");
    printintTyped(fp, node->ershov, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "datatype = ");
    printDataTypeTyped(fp, node->datatype, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "binary_op = ");
    printTokenTyped(fp, node->binary_op, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "binary_left = ");
    printExpressionTyped(fp, node->binary_left, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "binary_right = ");
    printExpressionTyped(fp, node->binary_right, depth);
    break;
  case UNARYEXPRESSION:
    fprintf(fp, "UnaryExpression");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "ershov = ");
    printintTyped(fp, node->ershov, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "datatype = ");
    printDataTypeTyped(fp, node->datatype, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "unary_op = ");
    printTokenTyped(fp, node->unary_op, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "unary_expression = ");
    printExpressionTyped(fp, node->unary_expression, depth);
    break;
  case NUMBERFACTOR:
    fprintf(fp, "NumberFactor");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "ershov = ");
    printintTyped(fp, node->ershov, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "datatype = ");
    printDataTypeTyped(fp, node->datatype, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "number_value = ");
    printintTyped(fp, node->number_value, depth);
    break;
  case BOOLEANFACTOR:
    fprintf(fp, "BooleanFactor");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "ershov = ");
    printintTyped(fp, node->ershov, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "datatype = ");
    printDataTypeTyped(fp, node->datatype, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "boolean_value = ");
    printboolTyped(fp, node->boolean_value, depth);
    break;
  case VARIABLEFACTOR:
    fprintf(fp, "VariableFactor");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "ershov = ");
    printintTyped(fp, node->ershov, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "datatype = ");
    printDataTypeTyped(fp, node->datatype, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "variable_symbol = ");
    printSymbolTyped(fp, node->variable_symbol, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "variable = ");
    printstringTyped(fp, node->variable, depth);
    break;
  case FUNCTIONFACTOR:
    fprintf(fp, "FunctionFactor");
    depth++;
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "ershov = ");
    printintTyped(fp, node->ershov, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "datatype = ");
    printDataTypeTyped(fp, node->datatype, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "function_symbol = ");
    printSymbolTyped(fp, node->function_symbol, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "function_name = ");
    printstringTyped(fp, node->function_name, depth);
    fprintf(fp, "\n");
    printIndent(fp, depth);
    fprintf(fp, "function_parameters = ");
    printExpressionListTyped(fp, node->function_parameters, depth);
    break;
  default:
    error(0, 0, "unknown option");
    exit(1);
    break;
  }
}

void printExpressionList(FILE *fp, struct ExpressionList *list, int depth) {
  if (NULL == list) return;
  fprintf(fp, "ExpressionList");
  depth++;
  struct ExpressionListElement *cur = list->head;
  while (NULL != cur) {
    fprintf(fp, "\n");
    printIndent(fp, depth);
    printExpression(fp, cur->node, depth);
    cur = cur->next;
  }
}

void printExpressionListTyped(FILE *fp, struct ExpressionList *list, int depth) {
  if (NULL == list) return;
  fprintf(fp, "ExpressionList");
  depth++;
  struct ExpressionListElement *cur = list->head;
  while (NULL != cur) {
    fprintf(fp, "\n");
    printIndent(fp, depth);
    printExpressionTyped(fp, cur->node, depth);
    cur = cur->next;
  }
}
