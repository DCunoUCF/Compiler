#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <error.h>
#include <assert.h>

#include "ast.h"
#include "symtab.h"
#include "codegen.h"
#include "vm_types.h"

// declarations
static void visitTranslationUnit(struct TranslationUnit *node);
static void visitBlock(struct Block *node);
static void visitVarDecls(struct TypedIdentList *list);
static void visitFuncDecls(struct FuncDeclList *list);
static void visitFuncDecl(struct FuncDecl *node);
static void visitFormals(struct TypedIdentList *list);

// statements
static void visitStatement(struct Statement *node);
static void visitAssignStatement(struct Statement *node);
static void visitCallStatement(struct Statement *node);
static void visitReturnStatement(struct Statement *node);
static void visitCompoundStatement(struct Statement *list);
static void visitIfStatement(struct Statement *node);
static void visitWhileStatement(struct Statement *node);
static void visitReadStatement(struct Statement *node);
static void visitWriteStatement(struct Statement *node);

// expressions
static int visitExpression(struct Expression *node, int reg_base);
static int visitBinaryExpressionInt(struct Expression *node, int reg_base);
static int visitUnaryExpressionInt(struct Expression *node, int reg_base);
static int visitBinaryExpressionBool(struct Expression *node, int reg_base);
static int visitUnaryExpressionBool(struct Expression *node, int reg_base);
static int visitNumberFactor(struct Expression *node, int reg_base);
static int visitBooleanFactor(struct Expression *node, int reg_base);
static int visitVariableFactor(struct Expression *node, int reg_base);
static int visitFunctionFactor(struct Expression *node, int reg_base);

// common functions for statements and expressions
static void setVariable(struct Symbol *symbol, int reg);
static void setupFunctionCall(struct Symbol *fsymbol, struct ExpressionList *parameters, int reg);

// helper functions for emitting ops
static int emit(Instruction instr);
static void backpatch(int branch_instruction_address, int new_disp);

static Instruction code[CODE_SIZE];
static int code_index;

static struct Scope *current_scope = NULL;

// helper macros for special registers
#define FP 12
#define SP 13
#define LN 14
#define IP 15

// stack frame offsets
#define OFFSET_FIRST_PARAM -4
#define OFFSET_RET_VAL -3
#define OFFSET_STATIC_LINK -2
#define OFFSET_RET_ADDR -1
#define OFFSET_FIRST_LOCAL 1

#include "codegen_tools.c"

void codegen(struct TranslationUnit *ast) {
  for (int i = 0; i < CODE_SIZE; i++) {
    code[i].op = OP_HLT;
    code[i].arg1 = 0;
    code[i].arg2 = 0;
    code[i].arg3 = 0;
  }
  code_index = 0;
  visitTranslationUnit(ast);
}

static void visitTranslationUnit(struct TranslationUnit *node) {
  current_scope = node->scope;
  visitBlock(node->block);
  code[code_index++] = VM_HLT();
}

static void visitBlock(struct Block *node) {
  // classroom exercise
  visitVarDecls(node->vardecls);

  // skip past function definitions, since we want to start running the current function body
  int saved_jump = emit(VM_BR(0));
  visitFuncDecls(node->funcdecls);

  // code_index is now the body of the function (or main)
  int entry_point = code_index;
  backpatch(saved_jump, code_index - saved_jump); // backpatch
  visitStatement(node->statement);
}

static void visitVarDecls(struct TypedIdentList *list) {
  struct TypedIdentListElement *cur = list->head;
  int num_vars = 0;
  while (NULL != cur) {
  	cur->node->symbol->address = num_vars + OFFSET_FIRST_LOCAL;
    code[code_index++] = VM_ADDI(SP, SP, 1);
    num_vars++;

    cur = cur->next;
  }
}

static void visitFuncDecl(struct FuncDecl *node) {
  struct Symbol *fsymbol = node->symbol;

  fsymbol->address = code_index;
  current_scope = node->scope;

  code[code_index++] = VM_PSH(LN, SP);
  code[code_index++] = VM_PSH(FP, SP);
  code[code_index++] = VM_MOV(FP, SP);
  visitFormals(node->formals);
  visitBlock(node->block); 
  code[code_index++] = VM_MOV(SP, FP);
  code[code_index++] = VM_POP(FP, SP);
  code[code_index++] = VM_POP(LN, SP);
  code[code_index++] = VM_RET(LN);

  current_scope = current_scope->parent;
}

static void visitFormals(struct TypedIdentList *list) {
  int num_formals = 0;
  struct TypedIdentListElement *cur = list->head;
  while (NULL != cur) {
    struct TypedIdent *node = cur->node;

    node->symbol->address = OFFSET_FIRST_PARAM - num_formals;
    num_formals++;

    cur = cur->next;
  }
}

static void visitFuncDecls(struct FuncDeclList *list) {
  // given
  struct FuncDeclListElement *cur = list->head;
  while (NULL != cur) {
    visitFuncDecl(cur->node);
    cur = cur->next;
  }
}

static void visitStatement(struct Statement *node) {
  // given
  switch (node->kind) {
  case ASSIGNSTATEMENT:
    visitAssignStatement(node);
    break;
  case CALLSTATEMENT:
    visitCallStatement(node);
    break;
  case RETURNSTATEMENT:
    visitReturnStatement(node);
    break;
  case COMPOUNDSTATEMENT:
    visitCompoundStatement(node);
    break;
  case IFSTATEMENT:
    visitIfStatement(node);
    break;
  case WHILESTATEMENT:
    visitWhileStatement(node);
    break;
  case READSTATEMENT:
    visitReadStatement(node);
    break;
  case WRITESTATEMENT:
    visitWriteStatement(node);
    break;
  default:
    error(0, 0, "unknown option");
    exit(1);
    break;
  }
}

static void setVariable(struct Symbol *symbol, int reg) {
  int diff = current_scope->level - symbol->scope->level;
  int offset = symbol->address;
  int i;
  if(diff == 0)
    code[code_index++] = VM_ST(reg, FP, offset);
  else {
    code[code_index++] = VM_PSH(FP, SP);

    for(i = 0; i < diff; i++)
      code[code_index++] = VM_LD(FP, FP, OFFSET_STATIC_LINK);

    code[code_index++] = VM_ST(reg, FP, offset);
    code[code_index++] = VM_POP(FP, SP);
  }
}

static void visitAssignStatement(struct Statement *node) {
  // given
  struct Symbol *symbol = node->assign_symbol;

  int assigned_reg = visitExpression(node->assign_expression, node->assign_expression->ershov - 1);

  setVariable(symbol, assigned_reg);
}

static void setupFunctionCall(struct Symbol *fsymbol, struct ExpressionList *parameters, int reg_base) {
  int i;
  if(parameters->size > 0)
  {
    code[code_index++] = VM_ADDI(SP, SP, parameters->size);

    struct ExpressionListElement *cur = parameters->head;
    int i = 0;
    while (NULL != cur)
    {
      int parameter_reg = visitExpression(cur->node, cur->node->ershov - 1);

      code[code_index++] = VM_ST(parameter_reg, SP, -i);
      
      i++;
      cur = cur->next;
    }
  }

  code[code_index++] = VM_ADDI(SP, SP, 1);

  if (fsymbol->scope->level == current_scope->level)  {
    code[code_index++] = VM_PSH(FP, SP);
  } else {

    int diff = (current_scope->level - 1) - fsymbol->scope->level;

    if (diff >= 0) {

      if(diff > 0)
      {
        code[code_index++] = VM_LD(reg_base, FP, OFFSET_STATIC_LINK);

        for(i = 0; i < diff; i++)
          code[code_index++] = VM_LD(reg_base, reg_base, OFFSET_STATIC_LINK);

        code[code_index++] = VM_PSH(reg_base, SP);
      }
    else {
        code[code_index++] = VM_LD(0, FP, OFFSET_STATIC_LINK);
        code[code_index++] = VM_PSH(0, SP);
    }
    } else {
      error(0, 0, "nesting depth of callee should not be more than one level deeper than caller");
      exit(1);
    }
  }

  int address_reg = fsymbol->address - code_index;
  code[code_index++] = VM_BL(address_reg);

  code[code_index++] = VM_SUBI(SP, SP, 1);
  code[code_index++] = VM_POP(reg_base, SP);

  if(parameters->size > 0)
    code[code_index++] = VM_SUBI(SP, SP, parameters->size);
}

static void visitCallStatement(struct Statement *node) {
  // given
  struct Symbol *fsymbol = node->call_symbol;
  struct ExpressionList *parameters = node->call_parameters;
  // since this is a statement, we know that no intermediate values
  // will be in registers.  so just use r0, though we don't need the
  // return value for a call statement
  int reg_base = 0;  // put result in r0
  setupFunctionCall(fsymbol, parameters, reg_base);
}

static void visitReturnStatement(struct Statement *node) {

  struct Symbol *fsymbol = node->function_symbol;
  int reg = visitExpression(node->return_expression, node->return_expression->ershov - 1);

  code[code_index++] = VM_ST(reg, FP, OFFSET_RET_VAL);
  code[code_index++] = VM_MOV(SP, FP);
  code[code_index++] = VM_POP(FP, SP);
  code[code_index++] = VM_POP(LN, SP);
  code[code_index++] = VM_RET(LN);
}

static void visitCompoundStatement(struct Statement *node) {
  // given
  struct StatementListElement *cur = node->compound_statement->head;
  while (NULL != cur) {
    visitStatement(cur->node);
    cur = cur->next;
  }
}

static void visitIfStatement(struct Statement *node) {
  // TODO

  // first evaluate the expression, recording the register in which the result is stored
	int reg = visitExpression(node->if_condition, node->if_condition->ershov - 1);
  // emit a comparison, i.e., cmpi reg 1
	emit(VM_CMPI(reg, 1));
  // emit a branch to skip the if body when comparison is false.  be
  // sure to store save the result of the emit, i.e., the index of the
  // branch instruction, for later backpatching.
	int stored_if_branch = emit(VM_BNE(0));
  // process the body of the if
	visitStatement(node->if_branch); // BODY?
  // emit a branch to skip the else body, again saving the index for
  // later backpatching.
	int stored_else_branch = emit(VM_BR(1));
  // backpatch the first branch (the one that skips the if body) to
  // jump to the current code index.  remember that branchs are
  // relative, i.e., don't backpatch it to the current code_index, but
  // instead the difference between the current code_index and the
  // index of the branch instruction.
	backpatch(stored_if_branch, code_index - stored_if_branch);
  // process the else branch if it exists
	if(node->if_elsebranch != NULL) {
		visitStatement(node->if_elsebranch); // BODY?

  		// backpatch the second branch (that skips the else body)
		backpatch(stored_else_branch, code_index - stored_else_branch);
	}
}

static void visitWhileStatement(struct Statement *node) {
	int start_index = code_index;

  // first evaluate the expression, recording the register in which the result is stored
	int reg = visitExpression(node->while_condition, node->while_condition->ershov - 1);

  // emit a comparison, i.e., cmpi reg 1
	emit(VM_CMPI(reg, 1));
  
  // emit a branch to skip the while body when comparison is false.  be
  // sure to store save the result of the emit, i.e., the index of the
  // branch instruction, for later backpatching.
	int stored_while_branch = emit(VM_BNE(0));

  // process the body of the while loop
	visitStatement(node->while_body);

  // emit branch backwards to while loop comparison
	emit(VM_BR(start_index - code_index));
  
  // backpatch the first branch (the one that skips the if body) to
  // jump to the current code index.  remember that branchs are
  // relative, i.e., don't backpatch it to the current code_index, but
  // instead the difference between the current code_index and the
  // index of the branch instruction.
	backpatch(stored_while_branch, (code_index - stored_while_branch)); 	
}

static void visitReadStatement(struct Statement *node) {
  emit(VM_READ(0));
  
	setVariable(node->read_symbol, 0);
}

static void visitWriteStatement(struct Statement *node) {
  // given
  int reg = visitExpression(node->write_expression, node->write_expression->ershov - 1);
  emit(VM_WR(reg));
}

static int visitExpression(struct Expression *node, int reg_base) {
  // given skeleton
  if (reg_base > 11 || (reg_base - node->ershov + 1) < 0) {
    /* Some expressions may require storing more intermediate results
       than there are available registers.  It is left as an optional exercise
       to allocate extra space in the stack frame for the temporary
       variables. */
    error(0, 0, "expression requires too many registers, please rewrite the source code using extra local variables");
    exit(1);
  }

  if (BINARYEXPRESSION == node->kind) {
    if (isInt(node->datatype)) {
      return visitBinaryExpressionInt(node, reg_base);
    } else if (isBool(node->datatype)) {
      return visitBinaryExpressionBool(node, reg_base);
    } else {
      assert(false); // should not happen if typechecker is correct
    }
  } else if (UNARYEXPRESSION == node->kind) {
    // classroom
    if (isInt(node->datatype)) {
      return visitUnaryExpressionInt(node, reg_base);
    } else if (isBool(node->datatype)) {
      return visitUnaryExpressionBool(node, reg_base);
    } else {
      assert(false); // should not happen if typechecker is correct
    }
  } else if (NUMBERFACTOR == node->kind) {
    return visitNumberFactor(node, reg_base);
  } else if (BOOLEANFACTOR == node->kind) {
    return visitBooleanFactor(node, reg_base);
  } else if (VARIABLEFACTOR == node->kind) {
    return visitVariableFactor(node, reg_base);
  } else if (FUNCTIONFACTOR == node->kind) {
    return visitFunctionFactor(node, reg_base);
  } else {
    error(0, 0, "unknown option");
    exit(1);
  }

}

static int visitBinaryExpressionInt(struct Expression *node, int reg_base) {
  // use ershov numbers to determine the base registers for the left
  // and right children

  int left_reg;
  int right_reg;
  
  if (node->binary_left->ershov == node->binary_right->ershov) {
    right_reg = visitExpression(node->binary_right, reg_base - 1);
  
    left_reg = visitExpression(node->binary_left, reg_base);
  
  } else if (node->binary_left->ershov > node->binary_right->ershov) {
    left_reg = visitExpression(node->binary_left, reg_base);
  
    right_reg = visitExpression(node->binary_right, reg_base);

  } else if (node->binary_left->ershov < node->binary_right->ershov) {
    right_reg = visitExpression(node->binary_right, reg_base);
  
    left_reg = visitExpression(node->binary_left, reg_base);
  }

  int result_reg = reg_base - node->ershov + 1;
  
  switch (node->binary_op->kind) {
    case PLUS:
      code[code_index++] = VM_ADD(result_reg, left_reg, right_reg);
      break;
    case MINUS:
      code[code_index++] = VM_SUB(result_reg, left_reg, right_reg);
      break;
    case MULT:
      code[code_index++] = VM_MUL(result_reg, left_reg, right_reg);
      break;
    case DIV:
      code[code_index++] = VM_DIV(result_reg, left_reg, right_reg);
      break;
    case MOD:
      code[code_index++] = VM_MOD(result_reg, left_reg, right_reg);
      break;
    default:
      assert(false);  // not supposed to happen if parser and typechecker are correct */
      break;
  }

  return result_reg;
}

static int visitUnaryExpressionInt(struct Expression *node, int reg_base) {
  int result_reg = visitExpression(node->unary_expression, reg_base);

  struct DataType *operand_type = node->unary_expression->datatype;

  switch (node->unary_op->kind) {
    case MINUS:
      code[code_index++] = VM_MULI(result_reg, result_reg, -1);
      break;
    default:
      assert(false);
      break;
  }
  return result_reg;
}

static int visitBinaryExpressionBool(struct Expression *node, int reg_base) {

  int left_reg;
  int right_reg;
  
  if (node->binary_left->ershov == node->binary_right->ershov) {
    right_reg = visitExpression(node->binary_right, reg_base - 1);
  
    left_reg = visitExpression(node->binary_left, reg_base);
  
  } else if (node->binary_left->ershov > node->binary_right->ershov) {
    left_reg = visitExpression(node->binary_left, reg_base);
  
    right_reg = visitExpression(node->binary_right, reg_base);

  } else if (node->binary_left->ershov < node->binary_right->ershov) {
    right_reg = visitExpression(node->binary_right, reg_base);
  
    left_reg = visitExpression(node->binary_left, reg_base);
  }

  int result_reg = reg_base - node->ershov + 1;

  switch (node->binary_op->kind) {
  case LT:
    code[code_index++] = VM_CMP(left_reg, right_reg);
    code[code_index++] = VM_BLT(3);
    code[code_index++] = VM_MOVI(result_reg, 0);
    code[code_index++] = VM_BR(2);
    code[code_index++] = VM_MOVI(result_reg, 1);
    break;
  case LTE:
    code[code_index++] = VM_CMP(left_reg, right_reg);
    code[code_index++] = VM_BLE(3);
    code[code_index++] = VM_MOVI(result_reg, 0);
    code[code_index++] = VM_BR(2);
    code[code_index++] = VM_MOVI(result_reg, 1);
    break;
  case GT:
    code[code_index++] = VM_CMP(left_reg, right_reg);
    code[code_index++] = VM_BGT(3);
    code[code_index++] = VM_MOVI(result_reg, 0);
    code[code_index++] = VM_BR(2);
    code[code_index++] = VM_MOVI(result_reg, 1);
    break;
  case GTE:
    code[code_index++] = VM_CMP(left_reg, right_reg);
    code[code_index++] = VM_BGE(3);
    code[code_index++] = VM_MOVI(result_reg, 0);
    code[code_index++] = VM_BR(2);
    code[code_index++] = VM_MOVI(result_reg, 1);
    break;
  case AND:
    code[code_index++] = VM_CMPI(left_reg, 0);
    code[code_index++] = VM_BEQ(5);
    code[code_index++] = VM_CMPI(right_reg, 0);
    code[code_index++] = VM_BEQ(3);
    code[code_index++] = VM_MOVI(result_reg, 1);
    code[code_index++] = VM_BR(2);
    code[code_index++] = VM_MOVI(result_reg, 0);
    break;
  case OR:
    code[code_index++] = VM_CMPI(left_reg, 1);
    code[code_index++] = VM_BEQ(5);
    code[code_index++] = VM_CMPI(right_reg, 1);
    code[code_index++] = VM_BEQ(3);
    code[code_index++] = VM_MOVI(result_reg, 0);
    code[code_index++] = VM_BR(2);
    code[code_index++] = VM_MOVI(result_reg, 1);
    break;
  case EQ:
    code[code_index++] = VM_CMP(left_reg, right_reg);
    code[code_index++] = VM_BEQ(3);
    code[code_index++] = VM_MOVI(result_reg, 0);
    code[code_index++] = VM_BR(2);
    code[code_index++] = VM_MOVI(result_reg, 1);
    break;
  case NEQ:
    code[code_index++] = VM_CMP(left_reg, right_reg);
    code[code_index++] = VM_BNE(3);
    code[code_index++] = VM_MOVI(result_reg, 0);
    code[code_index++] = VM_BR(2);
    code[code_index++] = VM_MOVI(result_reg, 1);
    break;
  default:
    assert(false);  // not supposed to happen if parser and typechecker are correct */
    break;
  }

  return result_reg;
}

static int visitUnaryExpressionBool(struct Expression *node, int reg_base) {
  // classroom exercise
  int result_reg = visitExpression(node->unary_expression, reg_base);
  struct DataType *operand_type = node->unary_expression->datatype;
  switch (node->unary_op->kind) {
  case NOT:
    emit(VM_CMPI(result_reg, 0));
    emit(VM_BEQ(3));
    emit(VM_MOVI(result_reg, 0));
    emit(VM_BR(2));
    emit(VM_MOVI(result_reg, 1));
    break;
  default:
    assert(false);  // not supposed to happen if parser and typechecker are correct */
    break;
  }
  return result_reg;
}

static int visitNumberFactor(struct Expression *node, int reg_base) {
  // given
  emit(VM_MOVI(reg_base, node->number_value));
  return reg_base;
}

static int visitBooleanFactor(struct Expression *node, int reg_base) {
  emit(VM_MOVI(reg_base, node->boolean_value));
  return reg_base;
}

static int visitVariableFactor(struct Expression *node, int reg_base) {
  int i = 0;
  struct Symbol *symbol = node->variable_symbol;
  
  int diff = current_scope->level - node->variable_symbol->scope->level;
  
  if(diff == 0) {
    code[code_index++] = VM_LD(reg_base, FP, node->variable_symbol->address);
  }
  else {
    code[code_index++] = VM_MOV(reg_base, FP);

    for(i = 0; i < diff; i++)
      code[code_index++] = VM_LD(reg_base, reg_base, OFFSET_STATIC_LINK);

    code[code_index++] = VM_LD(reg_base, reg_base, node->variable_symbol->address);
  }

  return reg_base;
}

static int visitFunctionFactor(struct Expression *node, int reg_base) {
  // given
  struct Symbol *fsymbol = node->function_symbol;
  struct ExpressionList *parameters = node->function_parameters;

  // push any registers that hold temporary values
  for (int i = 0; i < reg_base; i++) {
    code[code_index++] = VM_PSH(i, SP);
  }

  setupFunctionCall(fsymbol, parameters, reg_base);

  // pop any registers that hold temporary values
  for (int i = reg_base - 1; i >= 0; i--) {
    code[code_index++] = VM_POP(i, SP);
  }

  return reg_base;
}

static int emit(Instruction instr) {
  code[code_index] = instr;
  return code_index++;  // return the index of the instr, before incrementing it
}

static void backpatch(int branch_instruction_address, int new_disp) {
  code[branch_instruction_address].arg1 = new_disp;
}
