%{
    #include <stdlib.h>
    #include <stdio.h>
    #include "symboltable.h"
    entry_t** symbol_table;
    entry_t** constant_table;
    double Evaluate (double lhs_value,int assign_type,double rhs_value);
    int current_dtype;
    int yyerror(char *msg);
%}


%union
{
    double dval;
    entry_t* entry;
    int ival;
    char cval;
}


%token <entry> IDENTIFIER

 /* Constants */
%token <dval> DEC_CONSTANT HEX_CONSTANT FLOAT_CONSTANT
%token <cval> STRING_CONSTANT 
%token STRING

 /* Logical and Relational operators */
%token LOGICAL_AND LOGICAL_OR LS_EQ GR_EQ EQ NOT_EQ

 /* Short hand assignment operators */
%token MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN
%token LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN
%token INCREMENT DECREMENT

 /* Data types */
%token SHORT INT FLOAT CHAR LONG LONG_LONG SIGNED UNSIGNED CONST VOID

 /* Keywords */
%token IF FOR WHILE CONTINUE BREAK RETURN

%type <dval> expression
%type <dval> sub_expr
%type <dval> constant
%type <dval> unary_expr
%type <dval> arithmetic_expr
%type <dval> assignment_expr
%type <entry> lhs
%type <ival> assign_op

%start starter

%left ','
%right '='
%left LOGICAL_OR
%left LOGICAL_AND
%left EQ NOT_EQ
%left '<' '>' LS_EQ GR_EQ
%left '+' '-'
%left '*' '/' '%'
%right '!'


%nonassoc UMINUS
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE


%%

 /* Program is made up of multiple builder blocks. */
starter: starter builder
             |builder;

 /* Each builder block is either a function or a declaration */
builder: function|
       declaration;

 /* This is how a function looks like */
function: type IDENTIFIER '(' argument_list ')' compound_stmt;

 /* Now we will define a grammar for how types can be specified */

type :data_type pointer
    |data_type;

pointer: '*' pointer
    |'*'
    ;

data_type :sign_specifier type_specifier
    |type_specifier
    ;

sign_specifier :SIGNED
    |UNSIGNED
    ;

type_specifier :INT                  
    |FLOAT                             
    |CHAR                             
    |VOID                            
    |SHORT INT                        
    |SHORT                            
    |LONG                             
    |LONG INT                        
    |LONG_LONG                        
    |LONG_LONG INT                     
    ;

 /* grammar rules for argument list */
 /* argument list can be empty */
argument_list :arguments
    |
    ;
 /* arguments are comma separated TYPE ID pairs */
arguments :arguments ',' arg
    |arg
    ;

 /* Each arg is a TYPE ID pair */
arg :type IDENTIFIER
   ;

 /* Generic statement. Can be compound or a single statement */
stmt:compound_stmt
    |single_stmt
    ;

 /* The function body is covered in braces and has multiple statements. */
compound_stmt :'{' statements '}'
    ;

statements:statements stmt
    |
    ;

 /* Grammar for what constitutes every individual statement */
single_stmt :if_block
    |for_block
    |while_block
    |declaration
    |function_call ';'
    |RETURN ';'
    |CONTINUE ';'
    |BREAK ';'
    |RETURN sub_expr ';'
    |';'
    ;

for_block:FOR '(' expression_stmt1 expression_stmt2 expression_stmt3 ')' stmt
    ;


if_block:IF '(' expression ')' stmt %prec LOWER_THAN_ELSE
                |IF '(' expression ')' stmt ELSE stmt
    ;

while_block: WHILE '(' expression   ')' stmt
        ;

declaration:type declaration_list ';'
             |declaration_list ';'
             | unary_expr ';'

declaration_list: declaration_list ',' sub_decl
        |sub_decl;

sub_decl: assignment_expr
    |IDENTIFIER                     
    |array_index
    /*|struct_block ';'*/
    ;

/* This is because we can have empty expession statements inside for loops */
/*expression_stmt:expression ';'
    |';'
    ;
*/
expression:
    expression ',' sub_expr                            
    |sub_expr                                          
        ;

sub_expr:
    sub_expr '>' sub_expr                       
    |sub_expr '<' sub_expr                      
    |sub_expr EQ sub_expr                       
    |sub_expr NOT_EQ sub_expr                   
    |sub_expr LS_EQ sub_expr                    
    |sub_expr GR_EQ sub_expr                    
    |sub_expr LOGICAL_AND sub_expr             
    |sub_expr LOGICAL_OR sub_expr              
    |'!' sub_expr                              
    |arithmetic_expr                           
    |assignment_expr                          
    |unary_expr                                
    |IDENTIFIER                                     
    |constant                                
    |array_index
    ;


assignment_expr :lhs assign_op arithmetic_expr     
    |lhs assign_op array_index                     
    |lhs assign_op function_call                  
    |lhs assign_op unary_expr                     
    |unary_expr assign_op unary_expr             
    ;


unary_expr: lhs INCREMENT                         
    |lhs DECREMENT                            
    |DECREMENT lhs                               
    |INCREMENT lhs                                

lhs:IDENTIFIER                                   
    //|array_index
    ;

assign_op:'='                                      
    |ADD_ASSIGN                                   
    |SUB_ASSIGN                                 
    |MUL_ASSIGN                                    
    |DIV_ASSIGN                                   
    |MOD_ASSIGN                                   
    ;

arithmetic_expr: arithmetic_expr '+' arithmetic_expr    
    |arithmetic_expr '-' arithmetic_expr                
    |arithmetic_expr '*' arithmetic_expr                
    |arithmetic_expr '/' arithmetic_expr               
    |arithmetic_expr '%' arithmetic_expr                
    |'(' arithmetic_expr ')'                            
    |'-' arithmetic_expr %prec UMINUS                   
    |IDENTIFIER                                        
    |constant                                           
    ;

constant: DEC_CONSTANT                                  
    |HEX_CONSTANT                                       
    |FLOAT_CONSTANT                                    
    |STRING_CONSTANT                                    
    ;

expression_stmt1: assignment_expr ';'
    | ';'
;

expression_stmt2: sub_expr ';'
|';'
;

expression_stmt3: assignment_expr
    | unary_expr
    |
    ;

array_index: IDENTIFIER '[' sub_expr ']'

function_call: IDENTIFIER '(' parameter_list ')'
             |IDENTIFIER '(' ')'
             ;

parameter_list:
              parameter_list ','  parameter
              |parameter
              ;

parameter: sub_expr
                    |STRING

        ;
%%
#include "lex.yy.c"
#include <ctype.h>

int main(int argc, char *argv[])
{
    symbol_table = create_table();
    constant_table = create_table();
    yyin = fopen("test2.c", "r");
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
    printf("\n\Constant table");
    display(constant_table);
    fclose(yyin);
    return 0;
}
int yyerror(char *msg)
{
    printf("Line no: %d Error message: %s Token: %s\n", yylineno, msg, yytext);
}