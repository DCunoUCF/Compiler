#ifndef SYMTAB_H
#define SYMTAB_H

#include "ast.h"

struct Symbol {
  char *name;
  struct Scope *scope;
  struct DataType *datatype;
  unsigned int address;
};

struct SymbolList {
  struct SymbolListElement *head;
  struct SymbolListElement *tail;
};

struct SymbolListElement {
  struct Symbol *symbol;
  struct SymbolListElement *next;
};

struct Scope {
  char *name;
  struct Scope *parent;
  struct ScopeList *children;
  unsigned int level;
  struct SymbolList *symbols;
};

struct ScopeList {
  struct ScopeListElement *head;
  struct ScopeListElement *tail;
};

struct ScopeListElement {
  struct Scope *scope;
  struct ScopeListElement *next;
};

struct Scope *addScope(char *name, struct Scope *parent);

/**
  Return NULL is scope is the global scope.
*/
struct Scope *getParentScope(struct Scope *scope);

/**
   Return NULL if the symbol exists already.
 */
struct Symbol *addVariable(struct Scope *scope, char *name, struct TypedIdent *node);

/**
   Return NULL if the symbol exists already.
 */
struct Symbol *addFunction(struct Scope *scope, char *name, struct FuncDecl *node);

/**
   Searches the current scope for the symbol.  Returns NULL if the
   symbol is not found.
 */
struct Symbol *getSymbol(struct Scope *scope, char *name);

/**
  Recursively searches scopes starting from the current one.  Returns
  NULL if no variable found even after reaching the global scope.
*/
struct Symbol *searchSymbol(struct Scope *scope, char *name);

void print_space(FILE *fp, int depth);

void print_scope(FILE *fp, struct Scope *scope, int depth);

#endif
