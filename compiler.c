#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <getopt.h>

#include "token.h"
#include "lexer.h"

#include "ast.h"
#include "parser.h"
#include "typechecker.h"
#include "codegen.h"

enum action_e {
  A_LEX = 1,
  A_PARSE,
  A_TYPECHECK,
  A_CODEGEN,
};

int main(int argc, char **argv) {
  enum action_e action;
  int opt;

  // default to generating code from pl0 source
  action = A_CODEGEN;
  
  opterr = 0;
  while (1) {
    static struct option long_options[] = {
      {"lex", no_argument, 0, A_LEX},
      {"parse", no_argument, 0, A_PARSE},
      {"typecheck", no_argument, 0, A_TYPECHECK},
      {"codegen", no_argument, 0, A_CODEGEN},
      {0, 0, 0, 0}
    };

    int option_index = 0;

    opt = getopt_long(argc, argv, "", long_options, &option_index);

    if (-1 == opt) {
      break;
    } else {
      action = opt;
    }
  }

  if (!(optind < argc)) {
    fprintf(stderr, "missing filename\n");
    exit(1);
  }

  char *filename = argv[optind++];

  if (optind < argc) {
    fprintf(stderr, "too many arguments\n");
    exit(1);
  }

  FILE *infile;

  if (! (infile = fopen(filename, "r"))) {
    perror("fopen");
    exit(1);
  }

  struct Token **token_list;
  struct TranslationUnit *ast;

  token_list = lexer(infile);
  if (A_LEX == action) {
    write_tokens(stdout, token_list);
    exit(0);
  }

  ast = parser(token_list);
  if (action == A_PARSE) {
    printTranslationUnit(stdout, ast, 0);
    fprintf(stdout, "\n");
    exit(0);
  }

  typechecker(ast);
  if (action == A_TYPECHECK) {
    print_symtab(stdout);
    printTranslationUnitTyped(stdout, ast, 0);
    fprintf(stdout, "\n");
    exit(0);
  }

  codegen(ast);
  if (action == A_CODEGEN) {
    print_symtab(stderr);
    print_pcode(stdout);
  }
  
  return 0;
}
