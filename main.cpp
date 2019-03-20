#include <iostream>
#include <fstream>
#include "decaf_tokens.h"

extern std::ifstream in;
extern int yylineno;
extern int errors;

int yylex();

int main(int argc, char *argv[]) {
	if (argc != 2) {
		fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
		return 1;
	}

	in.open(argv[1], std::ios::in);

	if (!in) {
		std::cerr << "Cannot open file '" << argv[1] << "'\n";
		return 1;
	}

	errors = 0;
	yylineno = 1;
	yyparse();
}
