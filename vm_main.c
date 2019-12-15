#include <stdbool.h>
#include <stdio.h>
#include <errno.h>
#include <error.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#include "vm.h"

#include "vm_ops.c"

static void readInstructions(FILE *);

static bool debug = true;

int main(int argc, char **argv) {
  FILE *infile;

  if (argc != 2) {
    printf("USAGE: vm file.pcode < file.vmin > file.vmout 2> file.vmtrace\n");
    exit(0);
  }
  
  if (! (infile = fopen(argv[1], "r"))) {
    perror("fopen");
    exit(1);
  }

  readInstructions(infile);
  vm(stdin, stdout, stderr);

  return 0;
}

static void no_more_args(char *tok) {
  tok = strtok(NULL, " \n");
  if (NULL != tok) {
    error(0, 0, "too many args for op");
    exit(1);
  }
}

static uint32_t parse_reg(char *tok) {
  uint32_t arg;

  tok = strtok(NULL, " \n");
  if (NULL == tok) {
    error(0, 0, "invalid arg token");
    exit(1);
  }
  if (strlen(tok) < 2) {
    error(0, 0, "register name too short");
    exit(1);
  }
  if ('r' == tok[0]) {
    arg = atoi(tok+1);
    if (arg < 0 || arg >= NUM_REGS) {
      error(0, 0, "register number must be between 0 and 15 inclusive");
      exit(1);
    }
  } else if (strncmp(tok, "fp", 2) == 0) {
    arg = 12;
  } else if (strncmp(tok, "sp", 2) == 0) {
    arg = 13;
  } else if (strncmp(tok, "ln", 2) == 0) {
    arg = 14;
  } else if (strncmp(tok, "ip", 2) == 0) {
    arg = 15;
  } else {
    error(0, 0, "invalid register name");
    exit(1);
  }    

  return arg;
}

static uint32_t parse_imm(char *tok) {
  tok = strtok(NULL, " \n");
  if (NULL == tok) {
    error(0, 0, "invalid arg1 token");
    exit(1);
  }
  if (! (isdigit(tok[0]) || '-' == tok[0])) {
    error(0, 0, "expecting number");
    exit(1);
  }
  int in = atoi(tok);
  uint32_t arg = (uint32_t) in;
  // WARNING: not checking for overflow
  return arg;
}

static uint32_t parse_disp(char *tok) {
  uint32_t arg = parse_imm(tok);
  return arg;
}

void add_instruction(int count, uint8_t op, uint32_t arg1, uint32_t arg2, uint32_t arg3, char *str) {
  code[count].op = op;
  code[count].arg1 = arg1;
  code[count].arg2 = arg2;
  code[count].arg3 = arg3;
  code_string[count] = str;
}

void readInstructions(FILE *fp) {
  int count = 0;
  ssize_t read;
  size_t len = 0;
  char *line = NULL;
  char *save = NULL;

  for (int i = 0; i < CODE_SIZE; i++) {
    code[count].op = OP_HLT;
    code[count].arg1 = 0;
    code[count].arg2 = 0;
    code[count].arg3 = 0;
    code_string[count] = "";
  }

  uint8_t op;
  while (((read = getline(&line, &len, fp)) != -1)) {
    if (!(count < CODE_SIZE)) {
      fprintf(stderr, "exceeded pcode size\n");
      exit(1);
    }

    if (strlen(line) <= 1) {
      // skip blank lines
      continue;
    }

    if (line[0] == '#') {
      // skip comment lines
      continue;
    }

    char *tok;
    uint32_t arg1 = 0;
    uint32_t arg2 = 0;
    uint32_t arg3 = 0;
    char *str = malloc(sizeof(char *) * strlen(line) + 1);

    strcpy(str, line);
    str[strlen(str) - 1] = '\0';

    /* if (debug) printf("%s", line); */

    tok = strtok(line, " \n");

    if (NULL == tok) {
      error(0, 0, "invalid op token");
      exit(1);
    }

    // lookup op code
    struct opcode *tok_code = vm_ops(tok, strlen(tok));

    if (NULL == tok_code) {
      error(0, 0, "invalid op name");
      exit(1);
    }
    
    op = tok_code->num;
    
    switch (op) {
    case OP_NOP:  // no args
    case OP_HLT:
      no_more_args(tok);
      add_instruction(count++, op, arg1, arg2, arg3, str);
      break;
    case OP_READ: // ri
    case OP_WR:
    case OP_RET:
      arg1 = parse_reg(tok);
      no_more_args(tok);
      add_instruction(count++, op, arg1, arg2, arg3, str);
      break;
    case OP_BEQ: // disp
    case OP_BNE:
    case OP_BLT:
    case OP_BLE:
    case OP_BGT:
    case OP_BGE:
    case OP_BR:
    case OP_BL:
      arg1 = parse_disp(tok);
      no_more_args(tok);
      add_instruction(count++, op, arg1, arg2, arg3, str);
      break;
    case OP_MOV: // ri rj
    case OP_CMP:
    case OP_PSH:
    case OP_POP:
      arg1 = parse_reg(tok);
      arg2 = parse_reg(tok);
      no_more_args(tok);
      add_instruction(count++, op, arg1, arg2, arg3, str);
      break;
    case OP_MOVI: // ri imm
    case OP_CMPI:
      arg1 = parse_reg(tok);
      arg2 = parse_imm(tok);
      no_more_args(tok);
      add_instruction(count++, op, arg1, arg2, arg3, str);
      break;
    case OP_ADD: // dest/src ri rj
    case OP_SUB:
    case OP_MUL:
    case OP_DIV:
    case OP_MOD:
      arg1 = parse_reg(tok);
      arg2 = parse_reg(tok);
      arg3 = parse_reg(tok);
      no_more_args(tok);
      add_instruction(count++, op, arg1, arg2, arg3, str);
      break;
    case OP_ADDI: // dest/src ri imm
    case OP_SUBI:
    case OP_MULI:
    case OP_DIVI:
    case OP_MODI:
    case OP_LD:
    case OP_ST:
      arg1 = parse_reg(tok);
      arg2 = parse_reg(tok);
      arg3 = parse_imm(tok);
      no_more_args(tok);
      add_instruction(count++, op, arg1, arg2, arg3, str);
      break;
    default:
      error(0, 0, "unknown opcode");
      exit(1);
    }
  }
}
