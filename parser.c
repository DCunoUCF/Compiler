#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>

#include "lexer.h"
#include "ast.h"
#include "parser.h"

#include "parser_tools.c"

static struct TranslationUnit *program();
static struct  Block *block();
static struct  TypedIdentList *vardecls();
static struct  Token *type();
static struct  FuncDeclList *funcdecls();
static struct  TypedIdentList *formals();
static struct  Statement *statement();
static struct  ExpressionList *exprlist();
static struct  Expression *expr();
static struct  Expression *simpleexpr();
static struct  Expression *term();
static struct  Expression *factor();
bool static is_relop();
bool static is_termop();
bool static is_factorop();

struct TranslationUnit* parser(struct Token** token_list_in) {
  token_list = token_list_in;
  token_i = -1;

  return program();
}

// program      ::= block PERIOD
static struct  TranslationUnit *program() {
  // given
  struct TranslationUnit *node = newTranslationUnit();

  node->block = block();

  next();
  ensure_token(EOT);


  return node;
}

// block        ::= vardecls funcdecls statement
static struct  Block *block() {
  struct Block *node = newBlock();

  // TODO: classroom exercise
  node->vardecls = vardecls();
  node->funcdecls = funcdecls();
  node->statement = statement();

  return node;
}

// vardecls     ::= { VAR IDENT COLON type }
static struct  TypedIdentList *vardecls() {
  struct TypedIdentList *list = newTypedIdentList();

  // TODO: classroom exercise
	next();
	while (is_token(VAR)) {
		struct TypedIdent *node = newTypedIdent();

		next();
		ensure_token(IDENT);
		node->identifier = token()->identifier;

		next();
		ensure_token(COLON);

		// Capture type
		node->type = type(); // Returns an AST node leaf

		addTypedIdent(list, node);

		next();
	}

	previous();

  return list;
}

// funcdecls    ::= { FUNC IDENT LPAREN [ formals ] RPAREN [ COLON type ] block }
static struct  FuncDeclList *funcdecls() {
  struct FuncDeclList *list = newFuncDeclList();

  // TODO
	next();
	while (is_token(FUNC))
	{
		struct FuncDecl *node = newFuncDecl();

		next();
		ensure_token(IDENT);
		node->identifier = token()->identifier;
		node->formals = vardecls();

		next();
		ensure_token(LPAREN);

		next();
		if(is_token(IDENT)) {
			previous(); // because next(); is in formals for IDENT
			node->formals = formals();
		}
		else
			previous();

    next();
		ensure_token(RPAREN);
		
		next();
		if(is_token(COLON)) {
			node->return_type = type();
			node->has_return = true;
		} else {
			node->has_return = false;
			node->return_type = NULL;
			previous();
		}

		node->block = block();

		addFuncDecl(list, node);

		next();
	}

	previous();

  return list;
}

// formals      ::= IDENT COLON type { COMMA IDENT COLON type }
static struct  TypedIdentList *formals() {
  struct TypedIdentList *list = newTypedIdentList();

  // TODO
  struct TypedIdent *node = newTypedIdent();

  next();
  ensure_token(IDENT);
  node->identifier = token()->identifier;

  next();
  ensure_token(COLON);

  node->type = type();

  addTypedIdent(list, node);

  next();
  while(is_token(COMMA)) {
  	struct TypedIdent *node_while = newTypedIdent();

    next();
	  ensure_token(IDENT);
		node_while->identifier = token()->identifier;

  	next();
		ensure_token(COLON);

		node_while->type = type();
  	
		addTypedIdent(list, node_while);

		next();
  }

  previous();

  return list;
}

// type         ::= INT | BOOL
static struct  Token *type() {
  // given
  next();
  if (is_token(INT) || is_token(BOOL)) {
    return token();
  } else {
    parse_error();
    return NULL;
  }
}

/*
statement    ::= [ IDENT ASSIGN expr
                 | IDENT LPAREN exprlist RPAREN
                 | RETURN expression
                 | BEGIN { statement } END
                 | IF expr THEN statement [ELSE expr]
                 | WHILE expr DO statement
                 | READ IDENT
                 | WRITE expression
                 ]
*/
static struct  Statement *statement() {
  // skeleton given to select which statement
  
  next();
  if (is_token(IDENT)) {

    char *name = token()->identifier;

    next();
    if (is_token(ASSIGN)) {
      struct Statement *node = newStatement(ASSIGNSTATEMENT);

      // TODO
      node->assign_expression = expr();
      node->assign_variable = name;

      return node;
    } else if (is_token(LPAREN)) {
      struct Statement *node = newStatement(CALLSTATEMENT);

      // TODO
      node->call_parameters = exprlist();
      node->call_function = name;

      next();
      ensure_token(RPAREN);

      return node;
    } else {
      parse_error();
      return NULL;
    }
  } else if (is_token(RETURN)) {
    struct Statement *node = newStatement(RETURNSTATEMENT);

    // TODO
    node->return_expression = expr();

    return node;
  } else if (is_token(BEGIN)) {
    struct Statement *node = newStatement(COMPOUNDSTATEMENT);

    // TODO
    node->compound_statement = newStatementList();
    next();
    while(!is_token(END)) {
    	previous();
    	addStatement(node->compound_statement, statement());
    	next();
	}

	ensure_token(END);

    return node;
  } else if (is_token(IF)) {
    struct Statement *node = newStatement(IFSTATEMENT);

    // TODO
    node->if_condition = expr();

    next();
    ensure_token(THEN);

    node->if_branch = statement();

    next();
    if(is_token(ELSE)) {
    	node->if_elsebranch = statement();
    }
    else
    	previous();

    return node;
  } else if (is_token(WHILE)) {
    struct Statement *node = newStatement(WHILESTATEMENT);

    // TODO
    node->while_condition = expr();

    next();
    ensure_token(DO);

    node->while_body = statement();

    return node;
  } else if (is_token(READ)) {
    // given
    struct Statement *node = newStatement(READSTATEMENT);

    next();
    ensure_token(IDENT);
    node->read_variable = token()->identifier;

    return node;
  } else if (is_token(WRITE)) {
    // given
    struct Statement *node = newStatement(WRITESTATEMENT);

    node->write_expression = expr();
    return node;
  } else {
    parse_error();
    return NULL;
  }
}

// exprlist     ::= expr { COMMA expr }
static struct  ExpressionList *exprlist() {
  struct ExpressionList *node = newExpressionList();

  // TODO
  next();
  if (is_token(RPAREN)) {  // no arguments
    previous(); 
  } else { // has arguments
    previous();

    addExpression(node, expr());

    next();
    while (is_token(COMMA)) {
    	addExpression(node, expr());

    	next();
    }
    previous();
  }

  return node;
}

// expr         ::= simpleexpr [ relop simpleexpr ]
static struct  Expression *expr() {
  // TODO: classroom exercise
  // partially given to enable write NUMBER (allow user to work with tagged union)
   	
  struct Expression *left = simpleexpr();
  next();
  if (is_relop()) {
    struct Expression *node = newExpression(BINARYEXPRESSION);

    node->binary_left = left;
    node->binary_op = token();
    node->binary_right = simpleexpr();

    return node;
  } else {
    previous();
    return left;
  }
}

// relop        ::= LT | LTE | GT | GTE | EQ | NEQ
bool static is_relop() {
  //given
  switch (token()->kind) {
  case LT:
    // fall-through
  case LTE:
    // fall-through
  case GT:
    // fall-through
  case GTE:
    // fall-through
  case EQ:
    // fall-through
  case NEQ:
    return true;
  default:
    return false;
  }
}

// simpleexpr   ::= term [ termop term ]
static struct  Expression *simpleexpr() {
  // TODO (very similar to expr()

  struct Expression *left = term();

  next();
  if (is_termop()) {
    struct Expression *node = newExpression(BINARYEXPRESSION);

    node->binary_left = left;
    node->binary_op = token();
    node->binary_right = term();

    return node;
  } else {
    previous();
    return left;
  }
}

// termop       ::= PLUS | MINUS | OR
bool static is_termop() {
  // TODO (very similar to is_relop)
  switch (token()->kind) {
  case PLUS:
    // fall-through
  case MINUS:
    // fall-through
  case OR:
    return true;
  default:
    return false;
  }  // be sure to change this to be correct!
}

// term         ::= factor [ factorop factor ]
static struct  Expression *term() {
  // TODO (very similar to expr() and simpleexpr())
    	
	  struct Expression *left = factor();

  next();
  if (is_factorop()) {
    struct Expression *node = newExpression(BINARYEXPRESSION);

    // TODO
    node->binary_left = left;
    node->binary_op = token();
    node->binary_right = factor();

    return node;
  } else {
    previous();
    return left;
  } // be sure to change this to return a node!
}

// factorop     ::= MULT | DIV | MOD | AND
bool static is_factorop() {
  // TODO (very similar to is_relop and is_termop)
	  switch (token()->kind) {
  case MULT:
    // fall-through
  case DIV:
    // fall-through
  case MOD:
    // fall-through
  case AND:
    return true;
  default:
    return false;
  }  // be sure to change this to be correct!
}

/*
factor       - VariableFactor(variable) for IDENT
             | FunctionFactor(function_name, function_parameters) for 'IDENT LPAREN exprlist RPAREN'
             | UnaryExpression(unary_op, unary_expression) for 'NOT factor' or 'MINUS factor'
             | NumberFactor(number_value) for NUMBER
             | BooleanFactor(boolean_value) for TRUE or FALSE
             | pass through expr()'s return value for LPAREN expr RPAREN
*/
static struct  Expression *factor() {

    // TODO: similar to statement() checking for assignment vs. function call
    // be sure use previous correctly when you don't see the token you expect

    //return NULL; // be sure to change this to return a node!
   	
  next();
  if (is_token(IDENT)) {

    char *name = token()->identifier;

    next();
    if(is_token(LPAREN)) {
    	struct Expression *node = newExpression(FUNCTIONFACTOR);

    	node->variable = name;
    	
    	node->function_parameters = exprlist();

    	next();
    	ensure_token(RPAREN);

    	return node;
    } else {
    	previous();
    	struct Expression *node = newExpression(VARIABLEFACTOR);

    	node->variable = name;

    	return node;
    }
  } else if (is_token(NUMBER)) {
    // given
    struct Expression *node = newExpression(NUMBERFACTOR);

    node->number_value = token()->number;
    return node;
  } else if (is_token(TRUE)) {
    // given
    struct Expression *node = newExpression(BOOLEANFACTOR);
    
    node->boolean_value = true;
    return node;
  } else if (is_token(FALSE)) {
    // TODO (very similar to above)
    struct Expression *node = newExpression(BOOLEANFACTOR);

    node->boolean_value = false;
    return node; // be sure to change this to return a node!
  } else if (is_token(LPAREN)) {
    // TODO
  	struct Expression * node = expr();

  	next();
  	ensure_token(RPAREN);

    return node; // be sure to change this to return a node!
  } else if (is_token(NOT) || is_token(MINUS)) {
    // TODO
    struct Expression *node = newExpression(UNARYEXPRESSION);

    if(is_token(NOT)) {
    	node->unary_op = new_token(NOT);
    	node->unary_expression = expr();
    } else {
    	node->unary_op = new_token(MINUS);
    	node->unary_expression = expr();
    }

    return node; // be sure to change this to return a node!
  } else {
    parse_error();
    return NULL;
  }
}
