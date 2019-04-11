%{

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include "st1.h" 
//#include "y.tab.h"

entry_t** symbol_table; 
entry_t** constant_table; 
//int cmnt_strt = 0;

%}

%union
{
	double dval;
	entry_t* entry;
}

%token<entry> IDENTIFIER;

%token<dval> HEX_CONSTANT FLOAT_CONSTANT DEC_CONSTANT
%token STRING STRING_CONSTANT

%token LOGICAL_OR LOGICAL_AND EQ NOT_EQ LS_EQ GR_EQ
%token INCREMENT DECREMENT
%token ADD_ASSIGN DIV_ASSIGN MOD_ASSIGN SUB_ASSIGN MUL_ASSIGN

%token INT FLOAT VOID CHAR SIGNED UNSIGNED 

%token FOR BREAK WHILE CONTINUE IF ELSE RETURN

%type <dval> expression
%type <dval> sub_expr
%type <dval> assignment_expr
%type <dval> arithmetic_expr
%type <dval> unary_expr
%type <dval> constant
%type <entry> lhs

%start starter

%left ','
%left '='
%left LOGICAL_AND
%left LOGICAL_OR
%left '+' '-'
%left '*' '/' '%'

%nonassoc UMINUS
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE


%%
starter: starter builder | builder ;

builder: function | declaration ;

function: type IDENTIFIER '(' argument_list ')' com_stmt;

type: data_type pointer | data_type;

pointer: '*' pointer | '*' ;

data_type: sign_spec type_spec | type_spec ;

sign_spec: SIGNED | UNSIGNED;

type_spec: INT | FLOAT | CHAR | VOID;

argument_list: arguments | ;

arguments: arguments ',' arg | arg;

arg: type IDENTIFIER;

stmt: single_stmt | com_stmt;

statements: statements stmt | ;

com_stmt: '{' statements '}';

single_stmt: if_b | for_b | while_b | declaration| function_call ';' | RETURN ';' | CONTINUE ';' | BREAK ';' | RETURN sub_expr';' | ';'| ;

for_b: FOR '(' expression_stmt expression_stmt expression ')' stmt
     | FOR '(' expression_stmt expression_stmt ')' stmt;

if_b: 	IF '(' expression ')' stmt %prec LOWER_THAN_ELSE
      | IF '(' expression ')' stmt ELSE stmt;

while_b: WHILE '(' expression ')' stmt;

declaration: type declaration_list ';' | declaration_list ';' | unary_expr;

declaration_list: declaration_list ',' sub_decl | sub_decl | ;

sub_decl: assignment_expr | IDENTIFIER | array_access;

expression_stmt: expression | ';' ;

expression: expression ',' sub_expr | sub_expr;

sub_expr: sub_expr '>' sub_expr |  sub_expr '<' sub_expr | sub_expr LOGICAL_AND sub_expr | sub_expr LOGICAL_OR sub_expr | '!'sub_expr | assignment_expr | arithmetic_expr | unary_expr;

unary_expr: lhs INCREMENT | lhs DECREMENT | INCREMENT lhs | DECREMENT lhs;

assignment_expr: lhs assign_op function_call | lhs assign_op arithmetic_expr | lhs assign_op unary_expr | lhs assign_op array_access;

lhs: IDENTIFIER;

assign_op: '=' | MOD_ASSIGN | ADD_ASSIGN | DIV_ASSIGN | SUB_ASSIGN | MUL_ASSIGN;

arithmetic_expr: arithmetic_expr '+' arithmetic_expr |  arithmetic_expr '-' arithmetic_expr |  arithmetic_expr '*' arithmetic_expr |  arithmetic_expr '/' arithmetic_expr |  '(' arithmetic_expr ')' | IDENTIFIER | constant;

constant: HEX_CONSTANT | FLOAT_CONSTANT | DEC_CONSTANT;

array_access: IDENTIFIER '[' sub_expr ']';

function_call: IDENTIFIER '(' parameter_list ')' | IDENTIFIER '(' ')';

parameter_list: parameter_list ',' param | param ;

param: STRING | sub_expr;

%%

#include "lex.yy.c"
#include <ctype.h>

int main()
{
	symbol_table = create_table();
    constant_table = create_table();
    yyin = fopen("test1.c", "r");
    if(!yyparse())
    {
        printf("\nParsing complete\n");
    }
    else
    {
            printf("\nParsing failed\n");
    }
    printf("\n\tSymbol table");
    display(symbol_table);
    printf("\n\tConstant table");
    display(constant_table);
    fclose(yyin);
    return 0;
}

int yyerror(char *msg)
{
    printf("Line no: %d Error message: %s Token: %s\n", yylineno, msg, yytext);
}
