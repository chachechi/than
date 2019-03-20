%code requires {
    #include "ast.h"
}

%{
#include <iostream>

int yylex();

extern int yylineno;
extern char *yyfilename;
int errors;

void yyerror(const char *msg) {
    std::cerr << yylineno << ":" << msg << '\n';
    errors ++;
}

#define YYERROR_VERBOSE 1

%}

%expect 2

%union {
    Statement *statement_t;
    Expr *expr_t;
    char *str_t;
    int int_t;
    bool bool_t;
}


%token OP_ADD OP_SUB OP_MUL OP_DIV OP_ASSIGN
%token OP_EQ "=="
%token OP_NE "!="
%token OP_LE "<="
%token OP_GE ">="
%token OP_GT ">"
%token OP_LT "<"
%token OP_NEGATE "!"

%token TK_COMMA ","
%token TK_SEMICOLON ";"
%token TK_MOD "%"
%token TK_OR "||"
%token TK_AND "&&"
%token TK_SHL "<<"
%token TK_SHR ">>"

%token KW_INT "int"
%token KW_BOOL "bool"
%token KW_VOID "void"
%token<bool_t> KW_TRUE "true"
%token<bool_t> KW_FALSE "false"
%token KW_READ "System.in.read"
%token KW_PRINT "System.out.print"
%token KW_PRINTLN "System.out.println"
%token KW_RANDOM "Random.nextInt"
%token KW_IF	"if"
%token KW_WHILE	"while"
%token KW_FOR	"for"
%token KW_ELSE	"else"
%token KW_RETURN "return"
%token KW_BREAK "break"
%token KW_CONTINUE "continue"
%token KW_CLASS "class"

%token charConstant "charac"
%token stringConst "string"
%token<int_t> intConstant "number"
%token<str_t> TK_IDENT "identifier"
%token TK_EOL
%token TK_EOF "end of input"
%token TK_ERROR

%left TK_OR
%left TK_AND
%nonassoc OP_EQ OP_NE 
%nonassoc OP_LT OP_LE OP_GE OP_GT
%left TK_SHL TK_SHR
%left TK_MOD
%left OP_ADD OP_SUB
%left OP_MUL OP_DIV
%left OP_NEGATE

%%

/*input: "class" class_name '{' field_declare method_declare '}'

class_name: TK_IDENT

field_declare:  type TK_IDENT ';' field_declare
|               type TK_IDENT OP_ASSIGN constant ';' field_declare
|               

method_declare: type TK_IDENT '(' multiDeclaration ')' statement_block method_declare
|               "void" TK_IDENT '(' multiDeclaration ')' statement_block method_declare
|               "void" TK_IDENT '(' ')' statement_block method_declare
|

multiDeclaration:   type TK_IDENT multiDeclaration
|                   ',' type TK_IDENT multiDeclaration
|

statement_block : '{' var_decl statement '}'
;

var_decl:   type variable ';' var_decl
|

variable:   TK_IDENT variable
|       ',' TK_IDENT variable
|

type:   "int" 
|       "bool"

statement : assign_statement ';' statement
           | method_call ';' statement
           | if_statement	 statement
           | while_statement	 statement
           | for_statement	statement
           | return_statement ';'	statement 
           | break_statement ';'	statement 
           | continue_statement ';' statement
           | statement_block statement
           |
;

assign_statement:   lvalue OP_ASSIGN expr
;

break_statement: "break" 
;

continue_statement: "continue"
;

return_statement: "return" opt_return 
;

opt_return: expr 
        | 		
;

if_statement: "if" '(' expr ')' statement_block opt_else
;

while_statement: "while" '(' expr ')' statement_block
;

for_statement: "for" '(' assign_statement ';' expr ';' assign_statement ')' statement_block 
;

opt_else: "else" statement_block 
        | 	
;
*/

expr:       lvalue 
        |   method_call
        |   expr bin_op expr
        |   constant 
//        |   '-' expr 
        |   OP_NEGATE expr 
        |   '(' expr ')' 
;

method_call:    TK_IDENT '(' multipleExpr ')' 
|               "System.out.print" '(' argument ')'
|               "System.out.println" '(' argument ')' 
|               "System.in.read" '(' ')' 
|               "Random.nextInt" '(' argument ')' 

multipleExpr: expr multipleExpr
|   ',' expr multipleExpr
|

argument:   stringConst
|           expr

lvalue: TK_IDENT 
|       TK_IDENT '[' expr ']'


bin_op: arith_op | eq_op | rel_op | cond_op

arith_op: OP_ADD | OP_SUB | OP_MUL | OP_DIV | TK_SHL | TK_SHR | TK_MOD
rel_op: OP_LT | OP_LE | OP_GE | OP_GT 

eq_op:  OP_EQ | OP_NE

cond_op: TK_AND | TK_OR


constant:   intConstant
|           charConstant
|           bool_constant  

bool_constant:  "true"  
|               "false"  
