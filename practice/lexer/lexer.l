%{

#include <stdlib.h>
#include <stdio.h>
#include "symboltable.h"
#include "tokens.h"

entry_t** symbol_table;
entry_t** constant_table;
int cmnt_strt = 0;

%}

letter [a-zA-Z]
digit [0-9]
ws  [ \t\r\f\v]+
identifier (_|{letter})({letter}|{digit}|_){0,31}
hex [0-9a-f]

 /* Exclusive states */
%x CMNT
%x PREPROC

%%
  /* Keywords*/
"int"                             ;
"long"                            ;
"long long"                       ;
"short"                           ;
"signed"                          ;
"unsigned"                        ;
"for"                             ;
"break"                           ;
"continue"                        ;
"if"                              ;
"else"                            ;
"return"                          ;

{identifier}                      {
                                  insert( symbol_table,yytext,IDENTIFIER );}
{ws}                              ;
[+\-]?[0][x|X]{hex}+[lLuU]?        {insert( constant_table,yytext,HEX_CONSTANT);}
[+\-]?{digit}+[lLuU]?              {insert( constant_table,yytext,DEC_CONSTANT);}
[+\-]?{digit}*\.{digit}*           {insert( constant_table,yytext,FLOAT_CONSTANT);}
"/*"                              {cmnt_strt = yylineno; BEGIN CMNT;}
<CMNT>.|{ws}                      ;
<CMNT>\n                          {yylineno++;}
<CMNT>"*/"                        {BEGIN INITIAL;}
<CMNT>"/*"                        {printf("Line %3d: Nested comments are not valid!\n",yylineno);}
<CMNT><<EOF>>                     {printf("Line %3d: Unterminated comment\n", cmnt_strt); yyterminate();}
"#include<stdio.h>"                ;

"//".*                            ;

\"[^\"\n]*\"     {

  if(yytext[yyleng-2]=='\\') /* check if it was an escaped quote */
  {
    yyless(yyleng-1);       /* push the quote back if it was escaped */
    yymore();
  }
  else
  insert( constant_table,yytext,STRING);
 }

\"[^\"\n]*$                     {printf("Line %3d: Unterminated string %s\n",yylineno,yytext);}
{digit}+({letter}|_)+	        {printf("Line %3d: Illegal identifier name %s\n",yylineno,yytext);}
\n                              {yylineno++;}
"--"			                ;
"++"			                ;
"->"			               ;
"&&"			                ;
"||"			               ;
"<="			                ;
">="			               ;
"=="			               ;
"!="		                    ;
";"			                   ;
"{"                            ;
"}"                             ;
","			                  ;
"="			                    ;
"("			                    ;
")"			                   ;
"["                             ;
"]"                             ;
"+"			                    ;
"*"			                    ;
"/"		                        ;
"%"			                    ;
"<"			                    ;
">"			                    ;
.                               {printf("Line %3d: Illegal character %s\n",yylineno,yytext);}

%%

int main()
{
  yyin=fopen("test1.c","r");
  symbol_table=create_table();
  constant_table=create_table();
  yylex();
  printf("\n\tSymbol table");
  display(symbol_table);
  printf("\n\tConstants Table");
  display(constant_table);
  printf("NOTE: Please refer tokens.h for token meanings\n");
}