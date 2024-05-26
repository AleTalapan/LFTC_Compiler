#include <stdio.h>
#include <stdlib.h>
#include "utils.h"
#include "lexer.h"
#include "parser.h"
#include "ad.h"
#include "vm.h"

int main()
{
	char *inbuffer = loadFile("tests/testat.c");
    Token *tokens = tokenize(inbuffer);
    pushDomain();
    parse(tokens);
    //showDomain(symTable, "global");
    //dropDomain();
    free(inbuffer);
    //showTokens(tokens);
}