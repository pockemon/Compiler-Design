%{

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#include "y.tab.h"

entry_type** constant_table;

int cmnt_strt = 0;

%}

letter [a-zA-Z]
digit [0-9]
ws  [ \t\r\f\v]+
sp  [ \t]+
und [_]
id (_|{letter})({letter}|{digit}|_){0,31}
hex [0-9a-f]

 /* Exclusive states */
%x COMMENT
%x PREIN
%x PREDEF

%%
  /* Keywords*/
"int"                             {return INT;}
"char"                            {return CHAR;}
"char*"                           {return CHAR_STAR;}
"float"                           {return FLOAT;}
"void"                            {return VOID;}
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

{id}                      {return ID;}
{ws}                              ;
[+\-]?[0][x|X]{hex}+[lLuU]?       { yylval.node = insert(constant_table,yytext,(int)strtol(yytext, NULL, 16),INT); return  hexc;}
'({letter}|{digit})'              { yylval.node = insert(constant_table,yytext,yytext[1],CHAR); return charc;}
[+\-]?{digit}+[lLuU]?             { yylval.node = insert(constant_table,yytext,atoi(yytext),INT); return  decc;}
[+\-]?{digit}*\.{digit}+          { yylval.node = insert(constant_table,yytext,atof(yytext),FLOAT); return floatc;}

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
                                      yylval.node = insert(constant_table,yytext,INT_MAX,STRING);
                                      return STRING;
                                    }
                                  }

\"[^\"\n]*$                     {printf("Line %3d: Unterminated string %s\n",yylineno,yytext);}
{digit}+({letter}|_)+	          {printf("Line %3d: Illegal id name %s\n",yylineno,yytext);}
\n                              {yylineno++;}

"--"			                {return decr;}
"++"			                {return incr;}
"+="                      {return add_asn;}
"-="                      {return sub_asn;}
"*="                      {return mul_asn;}
"/="                      {return div_asn;}
"%="                      {return mod_asn;}


"&&"			                {return and_log;}
"||"			                {return or_log;}
"<="			                {return leq;}
">="			                {return geq;}
"=="			                {return eq;}
"!="		                  {return neq;}

.                         {return yytext[0];}

%%
