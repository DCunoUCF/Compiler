#include <malloc.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "symtab.h"
#include "datatype.h"

struct Scope *addScope(char *name, struct Scope *parent) {
  struct Scope *scope = malloc(sizeof(struct Scope));
  if (!scope) {
    perror("malloc");
    exit(1);
  }
  scope->name = name;
  scope->parent = parent;

  struct SymbolList *symbols = malloc(sizeof(struct SymbolList));
  if (!symbols) {
    perror("malloc");
    exit(1);
  }
  symbols->head = symbols->tail = NULL;
  scope->symbols = symbols;

  struct ScopeList *scopes = malloc(sizeof(struct ScopeList));
  if (!scopes) {
    perror("malloc");
    exit(1);
  }
  scopes->head = scopes->tail = NULL;
  scope->children = scopes;

  if (NULL != parent) {
    scope->level = parent->level + 1;
    struct ScopeListElement *elem = malloc(sizeof(struct ScopeListElement));
    if (!elem) {
      perror("malloc");
      exit(1);
    }
    elem->scope = scope;
    elem->next = NULL;
    if (NULL == parent->children->head) {
      parent->children->head = parent->children->tail = elem;
    } else {
      parent->children->tail->next = elem;
      parent->children->tail = parent->children->tail->next;
    }
  } else {
    scope->level = 0;
  }

  return scope;
}

struct Scope *getParentScope(struct Scope *scope) {
  return scope->parent;
}

static struct Symbol *addSymbol(struct Scope *scope, char *name, struct Symbol *symbol);

struct Symbol *addVariable(struct Scope *scope, char *name, struct TypedIdent *node) {
  if (NULL == getSymbol(scope, name)) {
    struct Symbol *symbol = malloc(sizeof(struct Symbol));
    if (!symbol) {
      perror("malloc");
      exit(1);
    }

    symbol->name = name;
    symbol->scope = scope;
    symbol->datatype = getPrimitiveType(node);
    symbol->address = 0;

    return addSymbol(scope, name, symbol);
  } else {
    return NULL;
  }
}

struct Symbol *addFunction(struct Scope *scope, char *name, struct FuncDecl *node) {
  if (NULL == getSymbol(scope, name)) {
    struct Symbol *symbol = malloc(sizeof(struct Symbol));
    if (!symbol) {
      perror("malloc");
      exit(1);
    }

    symbol->name = name;
    symbol->scope = scope;
    symbol->datatype = getFuncType(node);
    symbol->address = 0;

    return addSymbol(scope, name, symbol);
  } else {
    return NULL;
  }
}

static struct Symbol *addSymbol(struct Scope *scope, char *name, struct Symbol *symbol) {
  if (NULL != getSymbol(scope, name)) {
    return NULL;
  }
  
  struct SymbolListElement *elem = malloc(sizeof(struct SymbolListElement));
  if (!elem) {
    perror("malloc");
    exit(1);
  }

  /* fprintf(stderr, "adding %s\n", name); */
  elem->symbol = symbol;
  elem->next = NULL;

  if (NULL == scope->symbols->head) {
    scope->symbols->head = scope->symbols->tail = elem;
  } else {
    scope->symbols->tail->next = elem;
    scope->symbols->tail = scope->symbols->tail->next;
  }
    
  return symbol;
}

struct Symbol *getSymbol(struct Scope *scope, char *name) {
  struct SymbolListElement *list = scope->symbols->head;

  while (NULL != list) {
    int len = strlen(list->symbol->name);
    if (strlen(name) == len && 0 == strncmp(list->symbol->name, name, len)) {
      return list->symbol;
    }
    list = list->next;
  }

  return NULL;
}

struct Symbol *searchSymbol(struct Scope *scope, char *name) {
  if (NULL == scope) {
    return NULL;
  }

  struct Symbol *symbol = getSymbol(scope, name);

  if (NULL != symbol) {
    return symbol;
  } else {
    searchSymbol(scope->parent, name);
  }
}

void print_space(FILE *fp, int depth) {
  for (int i = 0; i < depth; i++) {
    fprintf(fp, "  ");
  }
}

void print_scope(FILE *fp, struct Scope *scope, int depth) {
  if (NULL == scope) {
    return;
  }

  print_space(fp, depth);
  fprintf(fp, "scope: %s, level: %d\n", scope->name, scope->level);
  
  depth++;

  struct SymbolListElement *elem = scope->symbols->head;
  while (NULL != elem) {
    print_space(fp, depth);
    fprintf(fp, "%s : ", elem->symbol->name);
    print_data_type(fp, elem->symbol->datatype);
    fprintf(fp, " @%d", elem->symbol->address);
    fprintf(fp, "\n");
    elem = elem->next;
  }

  struct ScopeListElement *container = scope->children->head;
  while (NULL != container) {
    print_scope(fp, container->scope, depth);
    container = container->next;
  }
}
