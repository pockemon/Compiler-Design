%{

#include <stdio.h>
#include "table.h"
#include "token_number.h"

entry_into_table** stable;
entry_into_table** ctable;
int cmnt_strt = 0;
%}

letter [a-zA-Z]
digit [0-9]
sp  [ \t\r\f\v]+
und [_]
identifier (_|{letter})({letter}|{digit}|_){0,31}
hex [0-9a-f]

 /* Exclusive states */
%x CMNT
%x PREIN
%x PREDEF

%%
  /* Keywords*/


"main(void)"                      {printf("\t%-10s : %2d\n",yytext,MAINFUNC);}
"main()"                          {printf("\t%-10s : %2d\n",yytext,MAINFUNC);}
"main(int argc, char **argv)"     {printf("\t%-10s : %2d\n",yytext,MAINFUNC);}
"main(int argc, char *argv[])"    {printf("\t%-10s : %2d\n",yytext,MAINFUNC);}
"int"                             {printf("\t%-10s : %2d\n",yytext,INT);}
"char"                            {printf("\t%-10s : %2d\n",yytext,CHAR);}
"float"                           {printf("\t%-10s : %2d\n",yytext,FLOAT);}
"void"                            {printf("\t%-10s : %2d\n",yytext,VOID);}
"long"                            {printf("\t%-10s : %2d\n",yytext,LONG);}
"long long"                       {printf("\t%-10s : %2d\n",yytext,LONG_LONG);}
"short"                           {printf("\t%-10s : %2d\n",yytext,SHORT);}
"signed"                          {printf("\t%-10s : %2d\n",yytext,SIGNED);}
"unsigned"                        {printf("\t%-10s : %2d\n",yytext,UNSIGNED);}
"for"                             {printf("\t%-10s : %2d\n",yytext,FOR);}
"while"                           {printf("\t%-10s : %2d\n",yytext,WHILE);}
"break"                           {printf("\t%-10s : %2d\n",yytext,BREAK);}
"continue"                        {printf("\t%-10s : %2d\n",yytext,CONTINUE);}
"if"                              {printf("\t%-10s : %2d\n",yytext,IF);}
"else"                            {printf("\t%-10s : %2d\n",yytext,ELSE);}
"return"                          {printf("\t%-10s : %2d\n",yytext,RETURN);}

{identifier}                      {printf("\t%-10s : %2d\n", yytext,IDENTIFIER);
                                  insert( stable,yytext,IDENTIFIER );}
{sp}                              ;
[+\-]?[0][x|X]{hex}+[lLuU]?       {printf("\t%-10s : %2d\n", yytext,HEX_CONSTANT);
									insert( ctable,yytext,HEX_CONSTANT);}
[+\-]?{digit}+[lLuU]?             {printf("\t%-10s : %2d\n", yytext,DEC_CONSTANT);
									insert( ctable,yytext,DEC_CONSTANT);}

"#ifdef"                        {printf("\t%-10s : %2d\n",yytext,IFDEF);}
"#ifndef"                       {printf("\t%-10s : %2d\n",yytext,IFNDEF);}
"#if"                           {printf("\t%-10s : %2d\n",yytext,IFF);}
"#else"                         {printf("\t%-10s : %2d\n",yytext,IELSE);}
"#elif"                         {printf("\t%-10s : %2d\n",yytext,IELIF);}
"#endif"                        {printf("\t%-10s : %2d\n",yytext,IENDIF);}
"#error"                        {printf("\t%-10s : %2d\n",yytext,ERROR);}
"#pragma"                       {printf("\t%-10s : %2d\n",yytext,PRAGMA);}

"/*"                            {cmnt_strt = yylineno; BEGIN CMNT;}
<CMNT>.|{sp}                      ;
<CMNT>\n                        {yylineno++;}
<CMNT>"*/"                      {BEGIN INITIAL;}
<CMNT>"/*"                      {printf("Line %2d: Nested comments are not valid!\n",yylineno);}
<CMNT><<EOF>>                   {printf("Line %2d: Unterminated comment\n", cmnt_strt); yyterminate();}
^"#include"                     {BEGIN PREIN;}
<PREIN>"<"[^<>\n]+">"           {printf("\t%-10s : %2d\n",yytext,HEADER_FILE);}
<PREIN>{sp}                       ;
<PREIN>\"[^"\n]+\"              {printf("\t%-10s : %2d\n",yytext,HEADER_FILE);}
<PREIN>\n                       {yylineno++; BEGIN INITIAL;}
<PREIN>.                        {printf("Line %2d: Illegal header file format \n",yylineno);}

^"#define"                       {BEGIN PREDEF;}
<PREDEF>{sp}+{letter}({letter}|{digit}|{und})*{sp}+{digit}+ {printf("\t%-10s : %2d\n",yytext,DEFINE_FILE);}
<PREDEF>{sp}+{letter}({letter}|{digit}|{und})*{sp}+({digit}+)\.({digit}+) {printf("\t%-10s : %2d\n",yytext,DEFINE_FILE);}
<PREDEF>{sp}+{letter}({letter}|{digit}|{und})*{sp}+{letter}({letter}|{digit}|{und})* {printf("\t%-10s : %2d\n",yytext,DEFINE_FILE);} 
<PREDEF>\n                       {yylineno++; BEGIN INITIAL;}
<PREDEF>.                        {printf("Line %2d: Illegal define file format \n",yylineno);}

"//".*                            ;

\"[^\"\n]*\"     {

  if(yytext[yyleng-2]=='\\') /* check if it was an escaped quote */
  {
    yyless(yyleng-1);       /* push the quote back if it was escaped */
    yymore();
  }
  else
  insert( ctable,yytext,STRING);
 }

\"[^\"\n]*$                  {printf("Line %2d: Unterminated string %s\n",yylineno,yytext);}
{digit}+({letter}|_)+	       {printf("Line %2d: Illegal identifier name %s\n",yylineno,yytext);}
\n                           {yylineno++;}
"--"			                   {printf("\t%-10s : %2d\n",yytext,DECREMENT);}
"++"			                   {printf("\t%-10s : %2d\n",yytext,INCREMENT);}
"->"			                   {printf("\t%-10s : %2d\n",yytext,PTR_SELECT);}
"&&"			                   {printf("\t%-10s : %2d\n",yytext,LOGICAL_AND);}
"||"			                   {printf("\t%-10s : %2d\n",yytext,LOGICAL_OR);}
"<="			                   {printf("\t%-10s : %2d\n",yytext,LS_THAN_EQ);}
">="			                   {printf("\t%-10s : %2d\n",yytext,GR_THAN_EQ);}
"=="			                   {printf("\t%-10s : %2d\n",yytext,EQ);}
"!="		                     {printf("\t%-10s : %2d\n",yytext,NOT_EQ);}
";"			                     {printf("\t%-10s : %2d\n",yytext,DELIMITER);}
"{"                          {printf("\t%-10s : %2d\n",yytext,OPEN_BRACES);}
"}"                          {printf("\t%-10s : %2d\n",yytext,CLOSE_BRACES);}
","			                     {printf("\t%-10s : %2d\n",yytext,COMMA);}
"="			                     {printf("\t%-10s : %2d\n",yytext,ASSIGN);}
"("			                     {printf("\t%-10s : %2d\n",yytext,OPEN_PAR);}
")"			                     {printf("\t%-10s : %2d\n",yytext,CLOSE_PAR);}
"["                          {printf("\t%-10s : %2d\n",yytext,OPEN_SQ_BRKT);}
"]"                          {printf("\t%-10s : %2d\n",yytext,CLOSE_SQ_BRKT);}
"-"			                     {printf("\t%-10s : %2d\n",yytext,MINUS);}
"+"			                     {printf("\t%-10s : %2d\n",yytext,PLUS);}
"*"			                     {printf("\t%-10s : %2d\n",yytext,STAR);}
"/"		                       {printf("\t%-10s : %2d\n",yytext,FW_SLASH);}
"%"			                     {printf("\t%-10s : %2d\n",yytext,MODULO);}
"<"			                     {printf("\t%-10s : %2d\n",yytext,LS_THAN);}
">"			                     {printf("\t%-10s : %2d\n",yytext,GR_THAN);}
"+="                         {printf("\t%-10s : %2d\n",yytext,ADDASS);}
"-="                         {printf("\t%-10s : %2d\n",yytext,SUBASS);}
"*="                         {printf("\t%-10s : %2d\n",yytext,MULASS);}
"/="                         {printf("\t%-10s : %2d\n",yytext,DIVASS);}
"%="                         {printf("\t%-10s : %2d\n",yytext,MODASS);}
.                            {printf("Line %2d: Illegal character %s\n",yylineno,yytext);}


%%

int main()
{
  stable=table_create();
  ctable=table_create();
  yyin=fopen("testcases/test-case-6.c","r");
  yylex();
  printf("\n\tSymbol table");
  display(stable);
  printf("\n\tConstant Table");
  display(ctable);
}
