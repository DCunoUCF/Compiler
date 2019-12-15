#ifndef VM_H
#define VM_H

#include <stdio.h>

#include "vm_types.h"

Instruction code[CODE_SIZE];
char *code_string[CODE_SIZE];

int vm(FILE*, FILE*, FILE*);

#endif
