%{
	#include <bits/stdc++.h>
	using namespace std;
	#include "symboltable.h"
	#include "lex.yy.c"

	void log_check(int,int);
	void log_res(icg_temp* & expr_left, icg_temp* param1, icg_temp* param2, const string& oper);
	void arith_check(int,int);
	void three_ad_code(icg_temp* & expr_left, icg_temp* param1, icg_temp* param2, const string& oper);
	void assi_check(int,int);
	void goto_fix(vector<int>&, int);
	void print_icg(string);
	int yyerror(char *msg);

	table_t stable[MAX_SCOPE];
	int declaration_status = 0,loop_status = 0,function_status = 0,function_type,cur_dtype,arg_list[10],arg_length = 0,func_composite_diff=0,assign_right_check = 0,follow_inst = 0,inter_var = 0;
	vector<string> output_icg_file;

%}

%union{	int dtype;entry_type* node;icg_temp* icg_cont;string* oper;vector<int>* follow_l;int instruct;}
%token SHORT INT LONG LONG_LONG SIGNED UNSIGNED CONST VOID CHAR FLOAT CHAR_STAR
%token <node> decc hexc charc floatc STRING ID
%token and_log or_log leq geq eq neq mul_asn div_asn mod_asn add_asn sub_asn incr decr IF FOR WHILE CONTINUE BREAK RETURN

%type <node> id const arr_ind
%type <oper> asn_opr;
%type <dtype> function_call
%type <instruct> find_next
%type <icg_cont> expr_left ind_expr expr expr_stmt expr_unary expr_arithm expr_assign arr if_stm for_stm while_stm composite_stmt stmts sole_stmt s_stmt direct_goto

%left ','
%right '='
%left or_log
%left and_log
%left eq neq
%left '<' '>' leq geq
%left '+' '-'
%left '*' '/' '%'
%right '!'
%nonassoc UM
%nonassoc LTE
%nonassoc ELSE

%start starter

%%

for_stm: FOR '(' expr_stmt find_next expr_stmt find_next expr ')' {loop_status = 1;} direct_goto find_next s_stmt {loop_status = 0;}
	         {
				$$ = new icg_temp();
				goto_fix($5->follow_if_correct,$11); goto_fix($12->follow_l,$6); goto_fix($12->follow_cont, $6); goto_fix($10->follow_l, $4);
				$$->follow_l = append_icg($5->follow_if_wrong,$12->follow_br);
				print_icg(string("goto ") + to_string($6));
			 }
    		 ;

while_stm: WHILE find_next '(' expr	')' find_next {loop_status = 1;} s_stmt {loop_status = 0;}
			{
				$$ = new icg_temp();
				goto_fix($8->follow_l,$2);	goto_fix($4->follow_if_correct,$6);	goto_fix($8->follow_cont, $2);
				$$->follow_l = append_icg($4->follow_if_wrong,$8->follow_br);
				print_icg(string("goto ") + to_string($2));
			}
		;

expr_stmt: expr ';'	 {
						$$ = new icg_temp(); 
						$$->follow_if_correct = $1->follow_if_correct; 	$$->follow_if_wrong = $1->follow_if_wrong;
					}
   				| ';'	{	$$ = new icg_temp();	}
    			;

expr: expr ',' ind_expr
				{
					$$ = new icg_temp();
					$$->follow_if_correct = $3->follow_if_correct; 	$$->follow_if_wrong = $3->follow_if_wrong;
				}
    		| ind_expr	
				{
					$$ = new icg_temp(); 
					$$->follow_if_correct = $1->follow_if_correct;	$$->follow_if_wrong = $1->follow_if_wrong;
				}
			;

ind_expr: ind_expr '>' ind_expr {	log_check($1->dtype,$3->dtype);	$$ = new icg_temp(); 	log_res($$, $1, $3, string(" > "));}
		| ind_expr '<' ind_expr { log_check($1->dtype,$3->dtype);	$$ = new icg_temp();	log_res($$, $1, $3, string(" < "));}
		| ind_expr eq ind_expr {	log_check($1->dtype,$3->dtype);	$$ = new icg_temp();	log_res($$, $1, $3, string(" == "));}
		| ind_expr neq ind_expr	{	log_check($1->dtype,$3->dtype);	$$ = new icg_temp();	log_res($$, $1, $3, string(" != "));}
		| ind_expr geq ind_expr	{	log_check($1->dtype,$3->dtype);	$$ = new icg_temp();	log_res($$, $1, $3, string(" >= "));}
		| ind_expr leq ind_expr	{	log_check($1->dtype,$3->dtype);	$$ = new icg_temp();	log_res($$, $1, $3, string(" <= "));}
		| ind_expr and_log find_next ind_expr {
				log_check($1->dtype,$4->dtype);
				$$ = new icg_temp();
				$$->dtype = $1->dtype;
				goto_fix($1->follow_if_correct,$3);
				$$->follow_if_correct = $4->follow_if_correct;
				$$->follow_if_wrong = append_icg($1->follow_if_wrong,$4->follow_if_wrong);
			}
		| ind_expr or_log find_next ind_expr{
				log_check($1->dtype,$4->dtype);
				$$ = new icg_temp();
				$$->dtype = $1->dtype;
				goto_fix($1->follow_if_wrong,$3);
				$$->follow_if_correct = append_icg($1->follow_if_correct,$4->follow_if_correct);
				$$->follow_if_wrong = $4->follow_if_wrong;
			}
		| '!' ind_expr {	$$ = new icg_temp();	$$->dtype = $2->dtype;	$$->follow_if_correct = $2->follow_if_wrong;	$$->follow_if_wrong = $2->follow_if_correct;}
		| expr_arithm {	$$ = new icg_temp(); 	$$->dtype = $1->dtype; 	$$->addr = $1->addr;}
    	| expr_assign {	$$ = new icg_temp(); 	$$->dtype = $1->dtype;}
		| expr_unary	 {	$$ = new icg_temp(); 	$$->dtype = $1->dtype;}
    ;

expr_unary:	id incr	{	$$ = new icg_temp();	$$->dtype = $1->dtype;	$$->code = string($1->lexeme) + string("++");	print_icg($$->code);}
 		  | id decr	{	$$ = new icg_temp();	$$->dtype = $1->dtype;	$$->code = string($1->lexeme) + string("--");	print_icg($$->code);}
	      | decr id	{	$$ = new icg_temp();	$$->dtype = $2->dtype;	$$->code = string("--") + string($2->lexeme);	print_icg($$->code);}
	      | incr id {	$$ = new icg_temp();	$$->dtype = $2->dtype;	$$->code = string("++") + string($2->lexeme);	print_icg($$->code);}


expr_assign : expr_left asn_opr expr_arithm	{	assi_check($1->node->dtype,$3->dtype);	$$ = new icg_temp();	$$->dtype = $3->dtype;	$$->code = $1->node->lexeme + *$2 + $3->addr;
				print_icg($$->code);	assign_right_check = 0;}
    		| expr_left asn_opr arr	{	assi_check($1->node->dtype,$3->dtype);	$$ = new icg_temp();	$$->dtype = $3->dtype;	$$->code = $1->node->lexeme + *$2 + $3->code;	print_icg($$->code);	assign_right_check = 0;	}
    		| expr_left asn_opr function_call {	assi_check($1->node->dtype,$3);	$$ = new icg_temp();	$$->dtype = $3;}
			| expr_left asn_opr expr_unary  {	assi_check($1->node->dtype,$3->dtype);	$$ = new icg_temp();	$$->dtype = $3->dtype;	$$->code = $1->node->lexeme + *$2 + $3->code;	print_icg($$->code); 	assign_right_check = 0;}
			| expr_unary asn_opr expr_unary	{	assi_check($1->dtype,$3->dtype);	$$ = new icg_temp();	$$->dtype = $3->dtype; 	$$->code = $1->code + *$2 + $3->code;	print_icg($$->code);	assign_right_check = 0;}
    ;

asn_opr:'=' 	{assign_right_check=1; $$ = new string(" = ");} |add_asn 	{assign_right_check=1; $$ = new string(" += ");}
    |sub_asn 	{assign_right_check=1; $$ = new string(" -= ");} |mul_asn 	{assign_right_check=1; $$ = new string(" *= ");} 
    |div_asn 	{assign_right_check=1;	$$ = new string(" /= ");} |mod_asn 	{assign_right_check=1; $$ = new string(" %= ");}
    ;

expr_arithm: expr_arithm '+' expr_arithm {	arith_check($1->dtype,$3->dtype);	$$ = new icg_temp();	$$->dtype = $1->dtype;	three_ad_code($$, $1, $3, string(" + "));}
			| expr_arithm '-' expr_arithm {	arith_check($1->dtype,$3->dtype);	$$ = new icg_temp();	$$->dtype = $1->dtype;	three_ad_code($$, $1, $3, string(" - "));}
			| expr_arithm '*' expr_arithm {	arith_check($1->dtype,$3->dtype);	$$ = new icg_temp();	$$->dtype = $1->dtype;	three_ad_code($$, $1, $3, string(" * "));}
			| expr_arithm '/' expr_arithm {	arith_check($1->dtype,$3->dtype);	$$ = new icg_temp();	$$->dtype = $1->dtype;	three_ad_code($$, $1, $3, string(" / "));}
		    | expr_arithm '%' expr_arithm {	arith_check($1->dtype,$3->dtype);	$$ = new icg_temp();	$$->dtype = $1->dtype;	three_ad_code($$, $1, $3, string(" % "));}
			|'(' expr_arithm ')' {	$$ = new icg_temp();	$$->dtype = $2->dtype;	$$->addr = $2->addr;	$$->code = $2->code;}
    		|'-' expr_arithm %prec UM {	$$ = new icg_temp();	$$->dtype = $2->dtype;	$$->addr = "t" + to_string(inter_var);	string expr = $$->addr + " = " + "minus " + $2->addr;	$$->code = $2->code + expr;	inter_var++;}
    	    |id {	$$ = new icg_temp();	$$->dtype = $1->dtype;	$$->addr = $1->lexeme;}
    		|const {	$$ = new icg_temp();	$$->dtype = $1->dtype;	$$->addr = to_string($1->value);}
    		 ;

const: decc {$1->is_constant=1; $$ = $1;} | hexc {$1->is_constant=1; $$ = $1;}| charc {$1->is_constant=1; $$ = $1;}| floatc	{$1->is_constant=1; $$ = $1;};

type : dtype pointer    {declaration_status = 1; }| dtype   {declaration_status = 1; };

pointer: '*' pointer| '*';

dtype : sign_specifier type_specifier | type_specifier;

sign_specifier : SIGNED | UNSIGNED;

type_specifier :INT  {cur_dtype = INT;} |SHORT  {cur_dtype = SHORT;} | LONG  {cur_dtype = LONG;} |LONG_LONG  {cur_dtype = LONG_LONG;} |CHAR  {cur_dtype = CHAR;} |FLOAT {cur_dtype = FLOAT;} |VOID	{cur_dtype = VOID;} ;


argument_list : arguments | ;

arguments : arguments ',' arg | arg ;

arg : type id	{
							arg_list[arg_length++] = $2->dtype;
							print_icg(string("arg ") + $2->lexeme);
						}
    ;

s_stmt:composite_stmt		{$$ = new icg_temp(); $$=$1;}
    |sole_stmt		{$$ = new icg_temp(); $$=$1;}
    ;

composite_stmt :'{' 	{
					if(!func_composite_diff)current_scope = create_new_scope();
					else func_composite_diff = 0;
				}				
				stmts				
				'}' 				
				{	current_scope = exit_scope();	$$ = new icg_temp();	$$ = $3;}
    ;

stmts:stmts find_next s_stmt	{	goto_fix($1->follow_l,$2);	$$ = new icg_temp();	$$->follow_l = $3->follow_l;	$$->follow_br = append_icg($1->follow_br,$3->follow_br);	$$->follow_cont = append_icg($1->follow_cont,$3->follow_cont);}
    |							{	$$ = new icg_temp();	}
    ;

sole_stmt :  if_stm	{	$$ = new icg_temp();	$$ = $1;	goto_fix($$->follow_l, follow_inst);}
		    |for_stm	{	$$ = new icg_temp();	$$ = $1;	goto_fix($$->follow_l, follow_inst);}
	    	|while_stm {	$$ = new icg_temp();	$$ = $1;	goto_fix($$->follow_l, follow_inst);}
	    	|declaration 		{$$ = new icg_temp();}
	    	|function_call ';'	{$$ = new icg_temp();}
			|RETURN ';'	  {	if(function_status)	{	if(function_type != VOID)	yyerror("return type (VOID) does not match function type");}
							else yyerror("return statement not inside function definition");
							}
	
			|CONTINUE ';'	{	if(!loop_status)	yyerror("Illegal use of continue");
								$$ = new icg_temp();	$$->follow_cont = {follow_inst};	print_icg("goto _");
							}
	
			|BREAK ';'      {	if(!loop_status) {yyerror("Illegal use of break");}
								$$ = new icg_temp();	$$->follow_br = {follow_inst};	print_icg("goto _");
						    }
	
			|RETURN ind_expr ';' { if(function_status){ if(function_type != $2->dtype)	yyerror("return type does not match function type");}
								   else yyerror("return statement not in function definition");}
	    ;

if_stm:IF '(' expr ')' find_next s_stmt 	%prec LTE {
				goto_fix($3->follow_if_correct,$5);	$$ = new icg_temp();	$$->follow_l = append_icg($3->follow_if_wrong,$6->follow_l);	$$->follow_br = $6->follow_br;	$$->follow_cont = $6->follow_cont;}

		|IF '(' expr ')' find_next s_stmt  ELSE direct_goto find_next s_stmt {
				goto_fix($3->follow_if_correct,$5);	goto_fix($3->follow_if_wrong,$9);	$$ = new icg_temp();	vector<int> temp = append_icg($6->follow_l,$8->follow_l);
				$$->follow_l = append_icg(temp,$10->follow_l);	$$->follow_br = append_icg($10->follow_br,$6->follow_br);	$$->follow_cont = append_icg($10->follow_cont,$6->follow_cont);
			}
    ;

declaration: type  declaration_list ';'			{declaration_status = 0;}| declaration_list ';'| expr_unary ';'

declaration_list: declaration_list ',' sub_decl	|sub_decl ;

sub_decl: expr_assign	|id	|arr ;

expr_left: id		{$$ = new icg_temp(); $$->node = $1;}
   | arr	{$$ = new icg_temp(); $$->code = $1->code;}
	 ;

id:ID   {
                    if(declaration_status && !assign_right_check){
                      $1 = insert(stable[current_scope].symbol_table,yytext,INT_MAX,cur_dtype);
                      if($1 == NULL) yyerror("Variable Re Declared");
                    }
                    else{
                      $1 = search_recursive(yytext);
                      if($1 == NULL) yyerror("Variable not declared");
                    }
                    
					$$ = $1;
                }
    		 ;

arr: id '[' arr_ind ']'	{	
						if(declaration_status)	{
							if($3->value <= 0)	yyerror("size of array is not positive");
							else if($3->is_constant)	$1->array_dimension = $3->value;
						}
					else if($3->is_constant){
						if($3->value > $1->array_dimension)		yyerror("Array index out of bound");
						if($3->value < 0)	yyerror("Array index cannot be negative");
						}
					$$ = new icg_temp();	$$->dtype = $1->dtype;		
					if($3->is_constant)		$$->code = string($1->lexeme) + string("[") + to_string($3->value) + string("]");
					else					$$->code = string($1->lexeme) + string("[") + string($3->lexeme) + string("]");
					$$->node = $1;
				}

arr_ind: const		{$$ = $1;}
		   | id		{$$ = $1;};

function_call: id '(' parameter_list ')'{	$$ = $1->dtype;	check_parameter_list($1,arg_list,arg_length);	arg_length = 0;	print_icg(string("function call ") + $1->lexeme);}
             | id '(' ')'	{	$$ = $1->dtype; 	check_parameter_list($1,arg_list,arg_length); 	arg_length = 0;	print_icg(string("function call ") + $1->lexeme);}
         ;

parameter_list:parameter_list ','  parameter
              |parameter
              ;

parameter: ind_expr	{	arg_list[arg_length++] = $1->dtype;	print_icg(string("param ") + $1->addr);}
		 | STRING	{	arg_list[arg_length++] = STRING;	print_icg(string("param ") + $1->lexeme);}
		 ;

find_next: 			{$$ = follow_inst;}
 ;

direct_goto:{	$$ = new icg_temp;	$$->follow_l = {follow_inst};	print_icg("goto _");}
	;


function: type id 	{function_type = cur_dtype;	declaration_status = 0;	current_scope = create_new_scope();	print_icg($2->lexeme + string(":"));}
		 '(' argument_list ')' 	
		 	{	declaration_status = 0;	fill_parameter_list($2,arg_list,arg_length);	arg_length = 0;	function_status = 1;	func_composite_diff=1;}
		 composite_stmt	{   function_status = 0;	}
		;

starter: starter builder
			 | builder;

builder: function
			 | declaration
			 ;
 

%%

void log_res(icg_temp* & expr_left, icg_temp* param1, icg_temp* param2, const string& oper)
{
	expr_left->dtype = param1->dtype;
	expr_left->follow_if_correct = {follow_inst};
	expr_left->follow_if_wrong = {follow_inst + 1};
	string code;
	code = string("if ") + param1->addr + oper + param2->addr + string(" goto _");	print_icg(code);
	code = string("goto _");	print_icg(code);
}

void three_ad_code(icg_temp* & expr_left, icg_temp* param1, icg_temp* param2, const string& oper)
{
	expr_left->addr = "t" + to_string(inter_var);
	string expr = expr_left->addr + string(" = ") + param1->addr + oper + param2->addr;
	expr_left->code = param1->code + param2->code + expr;
	inter_var++; print_icg(expr);
}

void goto_fix(vector<int>& vec, int num){
	int n = vec.size();
	for(int i = 0; i<n; i++){
		string instruction = output_icg_file[vec[i]];
		if(instruction.find("_") < instruction.size()) {	instruction.replace(instruction.find("_"),1,to_string(num));	output_icg_file[vec[i]] = instruction;}
	}
}

void print_icg(string x)
{
	string instruction;
	instruction = to_string(follow_inst) + string(":> ") + x;
	output_icg_file.push_back(instruction);	follow_inst++;
}

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

void displayICG()
{
	ofstream outfile("INT_CODE_GEN.code");
	for(int i=0; i<output_icg_file.size();i++)
	outfile << output_icg_file[i] <<endl;
	outfile << follow_inst << ":> exit";
	outfile.close();

}

int main()
{
	 int i;
	 for(i=0; i<MAX_SCOPE;i++)
	 {
	  stable[i].symbol_table = NULL;
	  stable[i].parent = -1;
	 }

	constant_table = create_table();
  stable[0].symbol_table = create_table();
	yyin = fopen("test.c", "r");

	if(!yyparse())
	{
		printf("\nPARSING COMPLETE\n\n\n");
	}
	else
	{
			printf("\nPARSING FAILED!\n\n\n");
	}

	displayICG();

	printf("SYMBOL TABLES\n\n");
	display_all();

}

int yyerror(const char *msg)
{
	printf("Line no: %d Error message: %s Token: %s\n", yylineno, msg, yytext);
	exit(0);
}
