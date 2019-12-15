void print_symtab(FILE *fp) {
  print_scope(fp, global_scope, 0);
}

void type_error(char *msg) {
  fprintf(stderr, "type error: %s\n", msg);
  exit(1);
}
