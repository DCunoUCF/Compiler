void print_instruction(FILE *fp, Instruction instruction);

void print_pcode(FILE *fp) {
  for (int i = 0; i < CODE_SIZE; i++) {
    Instruction instruction = code[i];
    print_instruction(fp, instruction);
    fprintf(fp, "\n");
    if (OP_HLT == instruction.op) {
      break;
    }
  }
}

char *register_name(int reg) {
  switch (reg) {
  case 0:
    return "r0";
  case 1:
    return "r1";
  case 2:
    return "r2";
  case 3:
    return "r3";
  case 4:
    return "r4";
  case 5:
    return "r5";
  case 6:
    return "r6";
  case 7:
    return "r7";
  case 8:
    return "r8";
  case 9:
    return "r9";
  case 10:
    return "r10";
  case 11:
    return "r11";
  case 12:
    return "fp";
  case 13:
    return "sp";
  case 14:
    return "ln";
  case 15:
    return "ip";
  default:
    return NULL;
  }
}

void print_instruction(FILE *fp, Instruction instr) {
  switch (instr.op) {
  case OP_NOP:
    fprintf(fp, "nop");
    break;
  case OP_HLT:
    fprintf(fp, "hlt");
    break;
  case OP_READ:
    fprintf(fp, "read");
    break;
  case OP_WR:
    fprintf(fp, "wr");
    break;
  case OP_RET:
    fprintf(fp, "ret");
    break;
  case OP_BEQ:
    fprintf(fp, "beq");
    break;
  case OP_BNE:
    fprintf(fp, "bne");
    break;
  case OP_BLT:
    fprintf(fp, "blt");
    break;
  case OP_BLE:
    fprintf(fp, "ble");
    break;
  case OP_BGT:
    fprintf(fp, "bgt");
    break;
  case OP_BGE:
    fprintf(fp, "bge");
    break;
  case OP_BR:
    fprintf(fp, "br");
    break;
  case OP_BL:
    fprintf(fp, "bl");
    break;
  case OP_CMP:
    fprintf(fp, "cmp");
    break;
  case OP_MOV:
    fprintf(fp, "mov");
    break;
  case OP_ADD:
    fprintf(fp, "add");
    break;
  case OP_SUB:
    fprintf(fp, "sub");
    break;
  case OP_MUL:
    fprintf(fp, "mul");
    break;
  case OP_DIV:
    fprintf(fp, "div");
    break;
  case OP_MOD:
    fprintf(fp, "mod");
    break;
  case OP_PSH:
    fprintf(fp, "psh");
    break;
  case OP_POP:
    fprintf(fp, "pop");
    break;
  case OP_CMPI:
    fprintf(fp, "cmpi");
    break;
  case OP_MOVI:
    fprintf(fp, "movi");
    break;
  case OP_ADDI:
    fprintf(fp, "addi");
    break;
  case OP_SUBI:
    fprintf(fp, "subi");
    break;
  case OP_MULI:
    fprintf(fp, "muli");
    break;
  case OP_DIVI:
    fprintf(fp, "divi");
    break;
  case OP_MODI:
    fprintf(fp, "modi");
    break;
  case OP_LD:
    fprintf(fp, "ld");
    break;
  case OP_ST:
    fprintf(fp, "st");
    break;
  default:
    error(0, 0, "unknown opcode");
    exit(1);
  }

  switch (instr.op) {
  case OP_READ: // ri
  case OP_WR:
  case OP_RET:
    fprintf(fp, " %s", register_name(instr.arg1));
    break;
  case OP_BEQ:  // disp
  case OP_BNE:
  case OP_BLT:
  case OP_BLE:
  case OP_BGT:
  case OP_BGE:
  case OP_BR:
  case OP_BL:
    fprintf(fp, " %d", instr.arg1);
    break;
  case OP_MOV:  // ri rj
  case OP_CMP:
  case OP_PSH:
  case OP_POP:
    fprintf(fp, " %s %s", register_name(instr.arg1), register_name(instr.arg2));
    break;
  case OP_MOVI:  // ri imm
  case OP_CMPI:
    fprintf(fp, " %s %d", register_name(instr.arg1), instr.arg2);
    break;
  case OP_ADD:  // dest/src ri rj
  case OP_SUB:
  case OP_MUL:
  case OP_DIV:
  case OP_MOD:
    fprintf(fp, " %s %s %s", register_name(instr.arg1), register_name(instr.arg2), register_name(instr.arg3));
    break;
  case OP_ADDI:  // dest/src ri imm
  case OP_SUBI:
  case OP_MULI:
  case OP_DIVI:
  case OP_MODI:
  case OP_LD:
  case OP_ST:
    fprintf(fp, " %s %s %d", register_name(instr.arg1), register_name(instr.arg2), instr.arg3);
    break;
  default:
    // no arguments; opcode already validated above.
    break;
  }
}

