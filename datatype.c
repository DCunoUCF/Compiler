#include <malloc.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <error.h>
#include <assert.h>

#include "ast.h"
#include "datatype.h"

static struct DataType primInt = { .kind = PRIMITIVETYPE, .primitive  = T_INT };
static struct DataType primBool = { .kind = PRIMITIVETYPE, .primitive = T_BOOL };
static struct DataType primVoid = { .kind = PRIMITIVETYPE, .primitive = T_VOID };

struct DataType *getInt() {
  return &primInt;
}

struct DataType *getBool() {
  return &primBool;
}

struct DataType *getVoid() {
  return &primVoid;
}

struct DataType *getPrimitiveType(struct TypedIdent *typedident) {
  switch (typedident->type->kind) {
  case BOOL:
    return getBool();
  case INT:
    return getInt();
  default:
    error(0, 0, "unkown token kind");
    exit(1);
  }
}

struct DataType *getFuncType(struct FuncDecl *funcdecl) {
  struct DataType *type = malloc(sizeof(struct DataType));
  if (NULL == type) {
    perror("malloc");
    exit(1);
  }
  type->kind = FUNCTIONTYPE;

  if (funcdecl->has_return) {
    switch (funcdecl->return_type->kind) {
    case BOOL:
      type->return_type = getBool();
      break;
    case INT:
      type->return_type = getInt();
      break;
    default:
      error(0, 0, "unkown token kind");
      exit(1);
    }
  } else {
    type->return_type = getVoid();
  }

  struct DataType *formals = malloc(sizeof(struct DataType));
  if (NULL == formals) {
    perror("malloc");
    exit(1);
  }
  formals->kind = TUPLETYPE;
  formals->tuple_head = formals->tuple_tail = NULL;

  struct TypedIdentListElement *cur = funcdecl->formals->head;
  while (NULL != cur) {
    struct TupleElement *param = malloc(sizeof(struct TupleElement));
    if (NULL == param) {
      perror("malloc");
      exit(1);
    }
    param->next = NULL;
    
    switch (cur->node->type->kind) {
    case BOOL:
      param->type = getBool();
      break;
    case INT:
      param->type = getInt();
      break;
    default:
      error(0, 0, "unkown token kind");
      exit(1);
    }

    if (NULL == formals->tuple_head) {
      formals->tuple_head = formals->tuple_tail = param;
    } else {
      formals->tuple_tail->next = param;
      formals->tuple_tail = formals->tuple_tail->next;
    }
    cur = cur->next;
  }
  type->params = formals;

  return type;
}

bool isPrimitiveType(struct DataType *datatype) {
  return PRIMITIVETYPE == datatype->kind;
}

bool isFuncType(struct DataType *datatype) {
  return FUNCTIONTYPE == datatype->kind;
}

bool isInt(struct DataType *datatype) {
  return PRIMITIVETYPE == datatype->kind && T_INT == datatype->primitive;
}

bool isBool(struct DataType *datatype) {
  return PRIMITIVETYPE == datatype->kind && T_BOOL == datatype->primitive;
}

bool isVoid(struct DataType *datatype) {
  return PRIMITIVETYPE == datatype->kind && T_VOID == datatype->primitive;
}

bool equalTypes(struct DataType *a, struct DataType *b) {
  if (a->kind != b->kind) {
    return false;
  } else if (PRIMITIVETYPE == a->kind) {
    return a->primitive == b->primitive;
  } else if (TUPLETYPE == a->kind) {
    struct TupleElement *param_a = a->tuple_head;
    struct TupleElement *param_b = b->tuple_head;

    while (NULL != param_a && NULL != param_b) {
      if (! equalTypes(param_a->type, param_b->type)) {
        return false;
      }

      param_a = param_a->next;
      param_b = param_b->next;
    }

    if (NULL != param_a || NULL != param_b) {
      return false;
    }
  } else if (FUNCTIONTYPE == a->kind) {
    if (! equalTypes(a->return_type, b->return_type)) {
      return false;
    }

    if (! equalTypes(a->params, b->params)) {
      return false;
    }

    return true;
  }
}

void print_data_type(FILE *fp, struct DataType *datatype) {
  switch (datatype->kind) {
  case PRIMITIVETYPE:
    switch (datatype->primitive) {
    case T_INT:
      fprintf(fp, "int");
      break;
    case T_BOOL:
      fprintf(fp, "bool");
      break;
    case T_VOID:
      fprintf(fp, "void");
      break;
    default:
      assert(false);
      break;
    }
    break;
  case TUPLETYPE:
    fprintf(fp, "(");
    struct TupleElement *param = datatype->tuple_head;
    while (NULL != param) {
      print_data_type(fp, param->type);
      param = param->next;
      if (NULL != param) {
        fprintf(fp, ", ");
      }
    }
    fprintf(fp, ")");
    break;
  case FUNCTIONTYPE:
    print_data_type(fp, datatype->params);
    fprintf(fp, " -> ");
    print_data_type(fp, datatype->return_type);
    break;
  default:
    assert(false);
  }
}
