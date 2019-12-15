#ifndef PARSER_H
#define PARSER_H

#include "ast.h"

struct TranslationUnit* parser(struct Token** token_list);

#endif
