static struct Token **token_list;
static int token_i;

static void next();
static void previous();
static struct Token *token();
static void parse_error();
static bool is_token(enum tok_kind kind);
static bool lookahead_token(enum tok_kind kind);
static bool ensure_token(enum tok_kind kind);

static void next() {
  token_i++;
  /* set_ast_line_col(token()->line, token()->col); */
  /* struct Token *token = token_list[token_i]; */
  /* if (NULL != token) { */
  /*   print_token(stderr, token); */
  /* } else { */
  /*   fprintf(stderr, "(end)"); */
  /* } */
  /* fprintf(stderr, "\n"); */
}

static void previous() {
  /* fprintf(stderr, "(back)\n"); */
  token_i--;
  /* if (token_i >= 0) { */
  /*   set_ast_line_col(token()->line, token()->col); */
  /* } */
}

static struct Token *token() {
  return token_list[token_i];
}

static void parse_error() {
  fprintf(stderr, "parse error on ");
  print_token(stderr, token());
  fprintf(stderr, "\n");
  exit(1);
}

static bool is_token(enum tok_kind kind) {
  return kind == token()->kind;
}

static bool lookahead_token(enum tok_kind kind) {
  return kind == token_list[token_i+1]->kind;
}

static bool ensure_token(enum tok_kind kind) {
  if (!is_token(kind)) {
    fprintf(stderr, "expected token: ");
    print_token_kind(stderr, kind);
    fprintf(stderr, "\n");
    parse_error();
  }
}
