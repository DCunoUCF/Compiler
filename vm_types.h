#ifndef VM_TYPES_H
#define VM_TYPES_H

#include <stdint.h>

typedef struct {
  uint8_t op;
  int32_t arg1;
  int32_t arg2;
  int32_t arg3;
} Instruction;

#define CODE_SIZE 0x1000

#define MEM_SIZE 0x10000

#define NUM_REGS 16

enum ops {
  OP_HLT = 0,
  OP_NOP,
  
  // memory
  OP_MOV,
  OP_MOVI,
  OP_LD,
  OP_ST,
  OP_PSH,
  OP_POP,

  // i/o
  OP_READ,
  OP_WR,

  // branching
  OP_BEQ,
  OP_BNE,
  OP_BLT,
  OP_BLE,
  OP_BGT,
  OP_BGE,
  OP_BR,
  OP_BL,
  OP_RET,

  // arithmetic
  OP_CMP,
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_MOD,
  OP_CMPI,
  OP_ADDI,
  OP_SUBI,
  OP_MULI,
  OP_DIVI,
  OP_MODI,
};

#endif
