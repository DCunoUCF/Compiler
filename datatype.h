#ifndef DATATYPE_H
#define DATATYPE_H

#include <stdbool.h>
#include <stdio.h>

#include "ast.h"

enum primitive_type {
  T_INT,
  T_BOOL,
  T_VOID,
};

struct TupleElement {
  struct DataType *type;
  struct TupleElement *next;
};

enum datatype_kind {
  PRIMITIVETYPE,
  TUPLETYPE,
  FUNCTIONTYPE,
};

struct DataType {
  enum datatype_kind kind;
  union {
    struct { // PRIMITIVETYPE
      enum primitive_type primitive;
    };

    struct { // TUPLETYPE
      struct TupleElement *tuple_head;
      struct TupleElement *tuple_tail;
    };

    struct { // FUNCTIONTYPE
      struct DataType *params;
      struct DataType *return_type;
    };
  };
};

struct DataType *getPrimitiveType(struct TypedIdent *typedident);

struct DataType *getFuncType(struct FuncDecl *funcdecl);

bool isPrimitiveType(struct DataType *datatype);

bool isFuncType(struct DataType *datatype);

bool isInt(struct DataType *datatype);

bool isBool(struct DataType *datatype);

bool isVoid(struct DataType *datatype);

struct DataType *getInt();

struct DataType *getBool();

struct DataType *getVoid();

bool equalTypes(struct DataType *a, struct DataType *b);

void print_data_type(FILE *fp, struct DataType *datatype);

#endif
