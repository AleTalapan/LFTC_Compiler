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
    vmInit();
    parse(tokens);
    Instr *testCode=genTestTema();
    run(testCode);
    //showDomain(symTable, "global");
    //dropDomain();
    free(inbuffer);
    //showTokens(tokens);
}