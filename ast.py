import yaml # python 3
from collections import defaultdict

order = []
regular = {}
listnodes = {}
unionnodes = {}

def gather(parent):
  for node in parent:
    order.append(node)
    data = parent[node]
    if data == "list":
      listnodes[node] = parent[node]
    else:
      if "union" in list(data.keys()):
        unionnodes[node] = data
        # gather(data["union"])
      else:
        regular[node] = data

with open('ast.yml', 'r') as f:
  doc = yaml.load(f)
  gather(doc)

header = {}
code = {}
print_code = {}

types = { "string": "char *", "bool": "bool ", "int": "int ", "Token": "struct Token *", "DataType": "struct DataType *", "Scope": "struct Scope *", "Symbol": "struct Symbol *" }
fields_for_typechecking = [ "scope", "symbol", "assign_symbol", "call_symbol", "function_symbol", "read_symbol", "variable_symbol", "datatype", "ershov" ]
fields = defaultdict(list)

for node in order:
  types[node] = "struct %s *" % (node)

for node in regular:
  data = regular[node]
  for fieldname in data:
    fieldnode = data[fieldname]
    if fieldnode not in types:
      print("not found", fieldnode)
      assert(False)
    fieldtype = types[fieldnode]
    fields[node].append((fieldnode, fieldtype, fieldname))

for node in regular:
  nodetype = types[node]
  decl = ""
  decl = decl + """
struct %s {""" % (node)
  for (fieldnode, fieldtype, fieldname) in fields[node]:
    decl = decl + """
  %s%s;""" % (fieldtype, fieldname)
  decl = decl + """
};
"""
  header[node] = decl

  impl = ""
  impl = impl + """
%snew%s() {
  %snode = malloc(sizeof(struct %s));
  if (NULL == node) {
    perror("malloc");
    exit(1);
  }
  return node;
}
""" % (nodetype, node, nodetype, node)
  code[node] = impl

  print_impl = ""
  print_impl = print_impl + """
void print%s(FILE *fp, %snode, int depth) {
  if (NULL == node) return;
  fprintf(fp, "%s");
  depth++;""" % (node, nodetype, node)
  for (fieldnode, fieldtype, fieldname) in fields[node]:
    if fieldname not in fields_for_typechecking:
      print_impl = print_impl + """
  fprintf(fp, "\\n");
  printIndent(fp, depth);
  fprintf(fp, "%s = ");
  print%s(fp, node->%s, depth);
""" % (fieldname, fieldnode, fieldname)
  print_impl = print_impl + """
}
"""

  print_impl = print_impl + """
void print%sTyped(FILE *fp, %snode, int depth) {
  if (NULL == node) return;
  fprintf(fp, "%s");
  depth++;""" % (node, nodetype, node)
  for (fieldnode, fieldtype, fieldname) in fields[node]:
    print_impl = print_impl + """
  fprintf(fp, "\\n");
  printIndent(fp, depth);
  fprintf(fp, "%s = ");
  print%sTyped(fp, node->%s, depth);
""" % (fieldname, fieldnode, fieldname)
  print_impl = print_impl + """
}
"""
  print_code[node] = print_impl

listinfo = {}
listtypes = {}
orignodes = {}

for node in listnodes:
  listtype = types[node]
  elemnode = "%sElement" % (node)
  elemtype = "struct %s *" % (elemnode)
  if not node.endswith("List"):
    assert(False)
  orignode = node[:-len("List")]
  orignodes[node] = orignode
  listtypes[orignode] = listtype
  origtype = types[orignode]
  if not orignode in types:
    assert(False)
  decl = ""
  decl = decl + """
struct %s {
  %shead;
  %stail;
  int size;
};

struct %s {
  %snode;
  %snext;
};
""" % (node, elemtype, elemtype, elemnode, origtype, elemtype)
  header[node] = decl

  impl = ""
  impl = impl + """
%snew%s() {
  %slist = malloc(sizeof(struct %s));
  if (NULL == list) {
    perror("malloc");
    exit(1);
  }
  list->head = NULL;
  list->tail = NULL;
  list->size = 0;
  return list;
}
""" % (listtype, node, listtype, node)

  impl = impl + """
%snew%s() {
  %selem = malloc(sizeof(struct %s));
  if (NULL == elem) {
    perror("malloc");
    exit(1);
  }
  elem->node = NULL;
  elem->next = NULL;
  return elem;
}
""" % (elemtype, elemnode, elemtype, elemnode)

  impl = impl + """
void add%s(%slist, %snode) {
  %selem = new%s();
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
""" % (orignode, listtype, origtype, elemtype, elemnode)
  code[node] = impl

  print_impl = ""
  print_impl = print_impl + """
void print%s(FILE *fp, %slist, int depth) {
  if (NULL == list) return;
  fprintf(fp, "%s");
  depth++;
  %scur = list->head;
  while (NULL != cur) {
    fprintf(fp, "\\n");
    printIndent(fp, depth);
    print%s(fp, cur->node, depth);
    cur = cur->next;
  }
}
""" % (node, listtype, node, elemtype, orignode);
  print_impl = print_impl + """
void print%sTyped(FILE *fp, %slist, int depth) {
  if (NULL == list) return;
  fprintf(fp, "%s");
  depth++;
  %scur = list->head;
  while (NULL != cur) {
    fprintf(fp, "\\n");
    printIndent(fp, depth);
    print%sTyped(fp, cur->node, depth);
    cur = cur->next;
  }
}
""" % (node, listtype, node, elemtype, orignode);
  print_code[node] = print_impl

for node in unionnodes:
  data = unionnodes[node]
  union = data["union"]
  options = union.keys()
  fields = set(data.keys()) - set(["union"])
  decl = ""

  decl = decl + """
enum %s_union {""" % (node.lower())
  for option in options:
    decl = decl + """
  %s,""" % (option.upper())
  decl = decl + """
};
"""
  
  decl = decl + """
struct %s {""" % (node)
  for field in fields:
    decl = decl + """
  %s %s;""" % (types[data[field]], field)
    
  decl = decl + """
  enum %s_union kind;
  union {""" % (node.lower())
  for option in options:
    decl = decl + """
    struct { //  %s""" % (option.upper())
    struct = union[option]
    for fieldname in struct:
      fieldnode = struct[fieldname]
      fieldtype = types[fieldnode]
      decl = decl + """
      %s%s;""" % (fieldtype, fieldname)
    decl = decl + """
    };"""
    # decl = decl + """
    # %s%s;""" % (types[option], option.lower())
  decl = decl + """
  };
};
"""
  header[node] = decl

  impl = ""
  impl = impl + """
%snew%s(enum %s_union kind) {
  %snode = malloc(sizeof(struct %s));
  if (NULL == node) {
    perror("malloc");
    exit(1);
  }
  node->kind = kind;
""" % (types[node], node, node.lower(), types[node], node)
  impl = impl + """
  return node;
}
"""
  code[node] = impl
  
  print_impl = ""
  print_impl = print_impl + """
void print%s(FILE *fp, %snode, int depth) {
  if (NULL == node) return;""" % (node, types[node])
  print_impl = print_impl + """
  switch (node->kind) {"""
  for option in options:
    print_impl = print_impl + """
  case %s:
    fprintf(fp, "%s");
    depth++;""" % (option.upper(), option)
    for fieldname in fields:
      fieldnode = data[fieldname]
      if fieldname not in fields_for_typechecking:
        print_impl = print_impl + """
    fprintf(fp, "\\n");
    printIndent(fp, depth);
    fprintf(fp, "%s = ");
    print%s(fp, node->%s, depth);""" % (fieldname, fieldnode, fieldname)
    struct = union[option]
    for fieldname in struct:
      fieldnode = struct[fieldname]
      fieldtype = types[fieldnode]
      if fieldname not in fields_for_typechecking:
        print_impl = print_impl + """
    fprintf(fp, "\\n");
    printIndent(fp, depth);
    fprintf(fp, "%s = ");
    print%s(fp, node->%s, depth);""" % (fieldname, fieldnode, fieldname)
    print_impl = print_impl + """
    break;"""
  print_impl = print_impl + """
  default:
    error(0, 0, "unknown option");
    exit(1);
    break;
  }
}
"""

  print_impl = print_impl + """
void print%sTyped(FILE *fp, %snode, int depth) {
  if (NULL == node) return;""" % (node, types[node])
  print_impl = print_impl + """
  switch (node->kind) {"""
  for option in options:
    print_impl = print_impl + """
  case %s:
    fprintf(fp, "%s");
    depth++;""" % (option.upper(), option)
    for field in fields:
      fieldnode = data[field]
      print_impl = print_impl + """
    fprintf(fp, "\\n");
    printIndent(fp, depth);
    fprintf(fp, "%s = ");
    print%sTyped(fp, node->%s, depth);""" % (field, fieldnode, field)
    struct = union[option]
    for fieldname in struct:
      fieldnode = struct[fieldname]
      fieldtype = types[fieldnode]
      print_impl = print_impl + """
    fprintf(fp, "\\n");
    printIndent(fp, depth);
    fprintf(fp, "%s = ");
    print%sTyped(fp, node->%s, depth);""" % (fieldname, fieldnode, fieldname)
    print_impl = print_impl + """
    break;"""
  print_impl = print_impl + """
  default:
    error(0, 0, "unknown option");
    exit(1);
    break;
  }
}
"""
  print_code[node] = print_impl

with open('ast.h', 'w') as f:
  f.write("""
#ifndef AST_H
#define AST_H

#include <stdio.h>

#include "token.h"
""")
  for node in order:
    if node in unionnodes.keys():
      f.write("""
enum %s_union;""" % (node.lower()))
    f.write("""
struct %s;""" % (node))

  f.write("""

#include "datatype.h""" + "\"")
    
  f.write("\n");
  for node in order:
    if (node in unionnodes.keys()):
      f.write("""
%snew%s(enum %s_union kind);""" % (types[node], node, node.lower()))
    else:
      f.write("""
%snew%s();""" % (types[node], node))

  f.write("\n");
  for node in listnodes:
    f.write("""
void add%s(%slist, %snode);""" % (orignodes[node], types[node], types[orignodes[node]]))

  f.write("""

void printIndent(FILE *fp, int depth);

void printstring(FILE *fp, char *string, int depth);
void printbool(FILE *fp, bool value, int depth);
void printint(FILE *fp, int value, int depth);
void printToken(FILE *fp, struct Token *token, int depth);
void printstringTyped(FILE *fp, char *string, int depth);
void printboolTyped(FILE *fp, bool value, int depth);
void printintTyped(FILE *fp, int value, int depth);
void printTokenTyped(FILE *fp, struct Token *token, int depth);
""")
  for node in order:
    f.write("""
void print%s(FILE *, %s, int );""" % (node, types[node]))
  f.write("\n")

  for node in order:
    f.write("""
void print%sTyped(FILE *, %s, int );""" % (node, types[node]))
  f.write("\n")
  for node in order:
    f.write(header[node])
  f.write("""
#endif
""")

with open('ast.c', 'w') as f:
  f.write("""
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
""")
  for node in order:
    f.write(code[node])
  f.write("\n")
  for node in order:
    f.write(print_code[node])
