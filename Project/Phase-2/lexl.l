%{

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
/* #include "symboltable.h" */
#include "y.tab.h"

/* entry_t** symbol_table; */
/* entry_t** constant_table; */
int cmnt_strt = 0;

%}

letter [a-zA-Z]
digit [0-9]
ws  [ \t\r\f\v]+
und [_]
identifier (_|{letter})({letter}|{digit}|_){0,31}
hex [0-9a-f]

 /* Exclusive states */
%x CMNT
%x PREIN
%x PREDEF

%%
  /* Keywords*/

"int"                             {return INT;}
"float"                             {return FLOAT;}
"char"                             {return CHAR;}
"void"                             {return VOID;}
"long"                            {return LONG;}
"long long"                       {return LONG_LONG;}
"short"                           {return SHORT;}
"signed"                          {return SIGNED;}
"unsigned"                        {return UNSIGNED;}
"for"                             {return FOR;}
"while"                           {return WHILE;}
"break"                           {return BREAK;}
"continue"                        {return CONTINUE;}
"if"                              {return IF;}
"else"                            {return ELSE;}
"return"                          {return RETURN;}

{identifier}                      {yylval.entry = insert(symbol_table, yytext, INT_MAX); return  IDENTIFIER;}
{ws}                              ;
[+\-]?{digit}*\.{digit}+ 			{yylval.dval =atof(yytext);insert( constant_table,yytext,FLOAT_CONSTANT );return FLOAT_CONSTANT;}
[+\-]?[0][x|X]{hex}+[lLuU]?        { yylval.dval = (int)strtol(yytext, NULL, 16);insert( constant_table,yytext,HEX_CONSTANT);return  HEX_CONSTANT;}
[+\-]?{digit}+[lLuU]?              {yylval.dval = atoi(yytext);insert( constant_table,yytext,DEC_CONSTANT); return  DEC_CONSTANT;}


"#ifdef"                           	{}
"#ifndef" 							{}
"#if" 								{}
"#else"        						{}
"#elif" 							{}
"#endif" 							{}
"#error" 							{}
"#pragma"                 			{}

"/*"                              {cmnt_strt = yylineno; BEGIN CMNT;}
<CMNT>.|{ws}                      ;
<CMNT>\n                          {yylineno++;}
<CMNT>"*/"                        {BEGIN INITIAL;}
<CMNT>"/*"                        {printf("Line %3d: Nested comments are not valid!\n",yylineno);}
<CMNT><<EOF>>                     {printf("Line %3d: Unterminated comment\n", cmnt_strt); yyterminate();}


"//".*                            ;

\"[^\"\n]*\"     {

  if(yytext[yyleng-2]=='\\') /* check if it was an escaped quote */
  {
    yyless(yyleng-1);       /* push the quote back if it was escaped */
    yymore();
  }
  else{
  insert( constant_table, yytext, INT_MAX);
  return STRING;
  }
 }

 \'{letter}\'     {


  insert( constant_table, yytext, INT_MAX);
  return STRING_CONSTANT;
 }

\"[^\"\n]*$                     {printf("Line %3d: Unterminated string %s\n",yylineno,yytext);}
{digit}+({letter}|_)+	        {printf("Line %3d: Illegal identifier name %s\n",yylineno,yytext);}
\n                              {yylineno++;}

"--"			                {return DECREMENT;}
"++"			                {return INCREMENT;}
 /* "->"			                {return PTR_SELECT;} */
"+="                      {return ADD_ASSIGN;}
"-="                      {return SUB_ASSIGN;}
"*="                      {return MUL_ASSIGN;}
"/="                      {return DIV_ASSIGN;}
"%="                      {return MOD_ASSIGN;}


"&&"			                {return LOGICAL_AND;}
"||"			                {return LOGICAL_OR;}
"<="			                {return LS_EQ;}
">="			                {return GR_EQ;}
"=="			                {return EQ;}
"!="		                  {return NOT_EQ;}
"="                       { return '='; }
.                         {return yytext[0];}

%%
/*
int main()
{
  yyin=fopen("test-case-1.c","r");

  constant_table=create_table();
  symbol_table = create_table();

  yylex();

  printf("\n\tSymbol table");
  display(symbol_table);
  printf("\n\tConstants Table");
  display(constant_table);

  printf("NOTE: Please refer tokens.h for token meanings\n");
} */
