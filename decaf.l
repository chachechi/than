%option noyywrap
%option yylineno
%x comment string charac

%{
#include <cstdio>
#include <iostream>
#include <fstream>
#include "decaf_tokens.h"

std::ifstream in;

#define YY_INPUT(buf, result, max_size) \
    do { \
        in.read(reinterpret_cast<char *>(buf), max_size); \
        result = in.gcount(); \
    } while (0)
    
#define YY_DECL int yylex()

void yyerror(const char *msg);
%}

%%
[ \t\n]   { /* Nada */ }
"\'"    { BEGIN(charac); }
"\""    { BEGIN(string); }
"/*"    { BEGIN(comment); }
"//".*\n { /* nada */ }
"+"		{ return OP_ADD; }
"-"		{ return OP_SUB; }
"*"		{ return OP_MUL; }
"/"		{ return OP_DIV; }
"||"	{ return TK_OR; }
"&&"	{ return TK_AND; }
"<<"	{ return TK_SHL; }
">>"	{ return TK_SHR; }
"("		{ return '('; }
")"		{ return ')'; }
"["		{ return '['; }
"]"		{ return ']'; }
"{"		{ return '{'; }
"}"		{ return '}'; }
"!"		{ return OP_NEGATE; }
"}"		{ return '}'; }
"="		{ return OP_ASSIGN; }
"=="	{ return OP_EQ; }
"!="	{ return OP_NE; }
"<"		{ return OP_LT; }
">"		{ return OP_GT; }
"<="	{ return OP_LE; }
">="	{ return OP_GE; }
";"		{ return TK_SEMICOLON; }
","		{ return TK_COMMA; }
"%"		{ return TK_MOD; }
"if"	{ return KW_IF; }
"while"	{ return KW_WHILE; }
"for"	{ return KW_FOR; }
"true"  { return KW_TRUE; }
"false"  { return KW_FALSE; }
"void"  { return KW_VOID; }
"int"  { return KW_INT; }
"bool"  { return KW_BOOL; }
"break"  { return KW_BREAK; }
"continue"  { return KW_CONTINUE; }
"return"  { return KW_RETURN; }
"else"  { return KW_ELSE; }
"class"  { return KW_CLASS; }
"System.out.print" { return KW_PRINT; }
"System.out.println" { return KW_PRINTLN; }
"System.in.read" { return KW_READ; }
"Random.nextInt" { return KW_RANDOM; }
[a-zA-Z_][a-zA-Z_0-9]* { yylval.str_t = strdup(yytext); return TK_IDENT; }
"0x"[0-9a-fA-F]+	{ yylval.int_t = atoi(yytext); return intConstant; }
[0-9]+	{ yylval.int_t = atoi(yytext); return intConstant; }
.		{ printf("0x%02x '%c' invalid character\n", (int)yytext[0], yytext[0]); return TK_ERROR; }

<comment>"*/" { BEGIN(INITIAL); }
<comment>.|\n { /* Nothing */ }
<comment><<EOF>> { yyerror("Block comment without end."); return 0; }

<string>"\"" { BEGIN(INITIAL); return stringConst; }
<string>. { /* Nothing */ }
<string><<EOF>> { yyerror("String without end."); return 0; }

<charac>[a-zA-Z]"\'" { BEGIN(INITIAL); return charConstant; }
<charac>"\\t""\'" { BEGIN(INITIAL); return charConstant; }
<charac>"\\r""\'" { BEGIN(INITIAL); return charConstant; }
<charac>"\\n""\'" { BEGIN(INITIAL); return charConstant; }
<charac>"\\'""\'" { BEGIN(INITIAL); return charConstant; }
<charac>"\\\"""\'" { BEGIN(INITIAL); return charConstant; }
<charac>"\\\\""\'" { BEGIN(INITIAL); return charConstant; }
<charac>. { yyerror("Invalid char."); return 0; }
<charac><<EOF>> { yyerror("Char without end."); return 0; }

%%
