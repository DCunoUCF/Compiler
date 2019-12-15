#ifndef CODEGEN_H
#define CODEGEN_H

#include "vm_types.h"

void codegen(struct TranslationUnit *node);

void print_pcode(FILE *fp);

#include "codegen_instr.h"

#endif
