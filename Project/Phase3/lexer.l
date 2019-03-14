%{

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#include "y.tab.h"
entry_type** ctable;

int cmnt_strt = 0;

%}

letter [a-zA-Z]
digit [0-9]
ws  [ \t\r\f\v]+
sp  [ \t]+
und [_]
identifier (_|{letter})({letter}|{digit}|_){0,31}
hex [0-9a-f]

 /* Exclusive states */
%x COMMENT
%x PREIN
%x PREDEF

%%
  /* Keywords*/
"int"                             {return INT;}
"char"                            {return CHAR;}
"float"                           {return FLOAT;}
"void"                            {return VOID;}
"long"                            {return LONG;}
"short"                           {return SHORT;}
"for"                             {return FOR;}
"while"                           {return WHILE;}
"break"                           {return BREAK;}
"continue"                        {return CONTINUE;}
"if"                              {return IF;}
"else"                            {return ELSE;}
"return"                          {return RETURN;}

{identifier}                      {return  ID;}
{ws}                              ;
[+\-]?[0][x|X]{hex}+[lLuU]?       { yylval.entry = insert(ctable,yytext,(int)strtol(yytext, NULL, 16),INT); return  hexc;}
'({letter}|{digit})'              { yylval.entry = insert(ctable,yytext,yytext[1],CHAR); return charc;}
[+\-]?{digit}+[lLuU]?             { yylval.entry = insert(ctable,yytext,atoi(yytext),INT); return  decc;}
[+\-]?{digit}*\.{digit}+          { yylval.entry = insert(ctable,yytext,atof(yytext),FLOAT); return floatc;}

"#ifdef"                        {printf("\t%-10s : - IFDEF\n",yytext);}
"#ifndef"                       {printf("\t%-10s : - IFNDEF\n",yytext);}
"#if"                           {printf("\t%-10s : - IFF\n",yytext);}
"#else"                         {printf("\t%-10s : - IELSE\n",yytext);}
"#elif"                         {printf("\t%-10s : - IELIF\n",yytext);}
"#endif"                        {printf("\t%-10s : - IENDIF\n",yytext);}
"#error"                        {printf("\t%-10s : - ERROR\n",yytext);}
"#pragma"                       {printf("\t%-10s : - PRAGMA\n",yytext);}

"/*"                            {cmnt_strt = yylineno; BEGIN COMMENT;}
<COMMENT>.|{sp}                      ;
<COMMENT>\n                        {yylineno++;}
<COMMENT>"*/"                      {BEGIN INITIAL;}
<COMMENT>"/*"                      {printf("Line %2d: Nested comments are not valid!\n",yylineno);}
<COMMENT><<EOF>>                   {printf("Line %2d: Unterminated comment\n", cmnt_strt); yyterminate();}
^"#include"                     {BEGIN PREIN;}
<PREIN>"<"[^<>\n]+">"           {printf("\t%-10s : - HEADER_FILE\n",yytext);}
<PREIN>{sp}                       ;
<PREIN>\"[^"\n]+\"              {printf("\t%-10s : - HEADER_FILE\n",yytext);}
<PREIN>\n                       {yylineno++; BEGIN INITIAL;}
<PREIN>.                        {printf("Line %2d: Illegal header file format \n",yylineno);}

^"#define"                       {BEGIN PREDEF;}
<PREDEF>{sp}+{letter}({letter}|{digit}|{und})*{sp}+{digit}+ {printf("\t%-10s :  DEFINE_FILE\n",yytext);}
<PREDEF>{sp}+{letter}({letter}|{digit}|{und})*{sp}+({digit}+)\.({digit}+) {printf("\t%-10s :  DEFINE_FILE\n",yytext);}
<PREDEF>{sp}+{letter}({letter}|{digit}|{und})*{sp}+{letter}({letter}|{digit}|{und})* {printf("\t%-10s :  DEFINE_FILE\n",yytext);} 
<PREDEF>\n                       {yylineno++; BEGIN INITIAL;}
<PREDEF>.                        {printf("Line %2d: Illegal define file format \n",yylineno);}


"//".*                            ;

\"[^\"\n]*\"                      {
                                    if(yytext[yyleng-2]=='\\') /* check if it was an escaped quote */
                                    {
                                      yyless(yyleng-1);       /* push the quote back if it was escaped */
                                      yymore();
                                    }
                                    else
                                    {
                                      insert(ctable,yytext,INT_MAX,STRING);
                                      return STRING;
                                    }
                                  }

\"[^\"\n]*$                     {printf("Line %3d: Unterminated string %s\n",yylineno,yytext);}
{digit}+({letter}|_)+	          {printf("Line %3d: Illegal identifier name %s\n",yylineno,yytext);}
\n                              {yylineno++;}

"--"			                {return decr;}
"++"			                {return incr;}
"+="                      {return add_asn;}
"-="                      {return sub_asn;}
"*="                      {return mul_asn;}
"/="                      {return div_asn;}
"%="                      {return mod_asn;}


"&&"			                {return and;}
"||"			                {return or;}
"<="			                {return leq;}
">="			                {return geq;}
"=="			                {return eq;}
"!="		                  {return neq;}

.                         {return yytext[0];}

%%
