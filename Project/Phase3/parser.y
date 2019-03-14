%{
	#include <stdio.h>

	void log_check(int,int);
	void arith_check(int,int);
	void assi_check(int,int);

	#include "symboltable.h"
	#include "lex.yy.c"	
	table_t stable[MAX_SCOPE];
	entry_type** ctable;

	int arg_list[10], arg_length = 0, func_composite_diff = 0 ,cur_dtype,assign_right_check = 0, declaration_status = 0, loop_status = 0, function_status = 0, function_type ;

	int yyerror(char *msg);
%}

%union
{
	int dtype;
	entry_type* entry;
}

%token SHORT INT LONG CONST VOID CHAR FLOAT
%token <entry> decc hexc charc floatc
%token IF FOR WHILE CONTINUE BREAK RETURN
%token <entry> ID
%token STRING
%token and or leq geq eq neq
%token mul_asn div_asn mod_asn add_asn sub_asn
%token incr decr
%type <entry> id const arr_index

%type <dtype> sub_expr unary_expr arithm_expr func_call assign_expr arr expr_left

%left ','
%right '='
%left or
%left and
%left eq neq
%left '<' '>' leq geq
%left '+' '-'
%left '*' '/' '%'
%right '!'
%nonassoc LTE
%nonassoc ELSE

%start starter


%%


for_stmt :FOR '(' expr_stmt  expr_stmt ')' {loop_status = 1;} stmt {loop_status = 0;}
    		 |FOR '(' expr_stmt expr_stmt expr ')' {loop_status = 1;} stmt {loop_status = 0;};

while_stmt : WHILE '(' expr	')' {loop_status = 1;} stmt {loop_status = 0;};

expr_stmt: expr ';' | ';' ;

expr: expr ',' sub_expr| sub_expr;

sub_expr: sub_expr '>' sub_expr	{log_check($1,$3); $$ = $1;} |sub_expr '<' sub_expr	{log_check($1,$3); $$ = $1;}
    |sub_expr eq sub_expr	{log_check($1,$3); $$ = $1;} |sub_expr neq sub_expr {log_check($1,$3); $$ = $1;}
    |sub_expr leq sub_expr {log_check($1,$3); $$ = $1;} |sub_expr geq sub_expr {log_check($1,$3); $$ = $1;}
	|sub_expr and sub_expr {log_check($1,$3); $$ = $1;} |sub_expr or sub_expr {log_check($1,$3); $$ = $1;}
	|'!' sub_expr {$$ = $2;} |arithm_expr {$$ = $1;} |assign_expr {$$ = $1;} |unary_expr {$$ = $1;}
    ;


assign_expr : expr_left asn_opr  arithm_expr {assi_check($1,$3); $$ = $3; assign_right_check=0;} | expr_left asn_opr  arr {assi_check($1,$3); $$ = $3;assign_right_check=0;}
    |expr_left asn_opr  func_call {assi_check($1,$3); $$ = $3;assign_right_check=0;}	| expr_left asn_opr  unary_expr {assi_check($1,$3); $$ = $3;assign_right_check=0;}
	|unary_expr asn_opr  unary_expr {assi_check($1,$3); $$ = $3;assign_right_check=0;}
    ;

unary_expr:	id incr {$$ = $1->dtype;}	| id decr	{$$ = $1->dtype;} | decr id {$$ = $2->dtype;}
	| incr id {$$ = $2->dtype;}
	;

expr_left: id	{$$ = $1->dtype;}   | arr	{$$ = $1;}
	 ;

asn_opr:'=' {assign_right_check=1;} |add_asn {assign_right_check=1;} |sub_asn {assign_right_check=1;} |mul_asn {assign_right_check=1;} |div_asn {assign_right_check=1;} |mod_asn {assign_right_check=1;}
    ;

arithm_expr: arithm_expr '+' arithm_expr {arith_check($1,$3);} |arithm_expr '-' arithm_expr {arith_check($1,$3);}
    |arithm_expr '*' arithm_expr {arith_check($1,$3);} |arithm_expr '/' arithm_expr {arith_check($1,$3);}
	|arithm_expr '%' arithm_expr {arith_check($1,$3);}	|'(' arithm_expr ')' {$$ = $2;} |id	{$$ = $1->dtype;} |const	{$$ = $1->dtype;}
    ;

declaration: type  declaration_list ';' {declaration_status = 0; } | declaration_list ';' | unary_expr ';'

declaration_list: declaration_list ',' sub_decl	|sub_decl ;

sub_decl: assign_expr |id |arr ;

id: ID { if(declaration_status && !assign_right_check) {
            $1 = insert(stable[current_scope].symbol_table,yytext,INT_MAX,cur_dtype);
            if($1 == NULL) yyerror("This variable is redeclared");
        }
        else {
            $1 = search_recursive(yytext);
            if($1 == NULL) yyerror("This variable not declared");
        }
        $$ = $1;
        }
    ;

const: decc {$1->is_constant=1; $$ = $1;}| hexc	{$1->is_constant=1; $$ = $1;} | charc {$1->is_constant=1; $$ = $1;}	| floatc {$1->is_constant=1; $$ = $1;}
    ;

stmts:stmts stmt | ;

stmt:composite_stmt|sole_stmt;

composite_stmt : '{' { if(!func_composite_diff) current_scope = create_new_scope(); else func_composite_diff = 0;}
		stmts
		'}' {current_scope = exit_scope();} ;


sole_stmt :if_stmt |for_stmt |while_stmt |declaration |func_call ';'
		|CONTINUE ';' {if(!loop_status) {yyerror("Illegal use of continue");}} |BREAK ';' {if(!loop_status) {yyerror("Illegal use of break");}}
		|RETURN ';'	{ if(function_status)
						{ 	if(function_type != VOID)
							yyerror("return type (VOID) does not match func_defination type");}

			  			else 
			  				yyerror("return statement not inside func_defination definition");
					}
		|RETURN sub_expr ';' {	if(function_status)	{
						if(function_type != $2)
						yyerror("return type does not match func_defination type");}
					else 
						yyerror("return statement not in func_defination definition");
			 		};

if_stmt :IF '(' expr ')' stmt %prec LTE |IF '(' expr ')' stmt ELSE stmt ;

type : dtype ptr {declaration_status = 1; } | dtype {declaration_status = 1; } ;

ptr: '*' ptr | '*' 	;

dtype :INT {cur_dtype = INT;} |SHORT  {cur_dtype = SHORT;} |LONG  {cur_dtype = LONG;} |CHAR {cur_dtype = CHAR;} |FLOAT {cur_dtype = FLOAT;} 
		|VOID	{cur_dtype =VOID;};


func_defination: type id {	function_type = cur_dtype; declaration_status = 0;	current_scope = create_new_scope();}
	'(' arg_list ')' {	func_composite_diff=1;fill_parameter_list($2,arg_list,arg_length); arg_length = 0; function_status = 1;declaration_status = 0;}
	composite_stmt { function_status = 0;	};


arg_list : arg_list ',' arg | arg | ;

arg : type id {arg_list[arg_length++] = $2->dtype;};

func_call: id '(' parameter_list ')'{ $$ = $1->dtype;
				verify_arg_list($1,arg_list,arg_length);
				arg_length = 0;	}

        | id '(' ')'	{ $$ = $1->dtype;
				verify_arg_list($1,arg_list,arg_length);
				arg_length = 0;	} ;

parameter_list: parameter_list ','  parameter  | parameter ;

parameter: sub_expr	{arg_list[arg_length++] = $1;} | STRING {arg_list[arg_length++] = STRING;} ;

arr: id '[' arr_index ']'{
			if(declaration_status)
			{
				if($3->value <= 0)
					yyerror("size of array is not positive");
				else
                if($3->is_constant && !assign_right_check)
					$1->array_dimension = $3->value;
				else if(assign_right_check){
					if($3->value > $1->array_dimension)
						yyerror("Array index out of bound");
					if($3->value < 0)
						yyerror("Array index cannot be negative");
				}
			}

			else if($3->is_constant)
			{
				if($3->value > $1->array_dimension)
					yyerror("Array index out of bound");

				if($3->value < 0)
					yyerror("Array index cannot be negative");
			}
			$$ = $1->dtype;
		}

arr_index: const	{$$ = $1;} | id	{$$ = $1;} ;

starter: starter block | block ;

block: func_defination | declaration ;


%%

void log_check(int left, int right)
{
	if(left != right) yyerror("Mismatch of data types in logical expr");
}

void arith_check(int left, int right)
{
	if(left != right) yyerror("Mismatch of data types in arithmetic expr");
}
void assi_check(int left, int right)
{
	if(left != right) yyerror("Mismatch of data types in assignment expr");
}

int main(int argc, char *argv[])
{
	 int i;
	 for(i=0; i<MAX_SCOPE;i++){
	  stable[i].symbol_table = NULL;
	  stable[i].parent = -1;
	 }

	ctable = create_table();
  	stable[0].symbol_table = create_table();
	yyin = fopen("test6.c", "r");

	if(!yyparse()) printf("\nParsing completed successfully\n\n\n");
	else printf("\nParsing not completed\n\n\n");

	printf("SYMBOL TABLE VIEW\n\n");
	stable_disp();

	printf("CONSTANT TABLE VIEW");
	ctable_disp(ctable);

	fclose(yyin);
	return 0;
}

int yyerror(char *msg)
{
	printf("Line no: %d Error message: %s Token: %s\n", yylineno, msg, yytext);
	exit(0);
}
