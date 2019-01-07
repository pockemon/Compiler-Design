%{
#include<stdio.h>
int ch=0, bl=0, ln=0, wr=0;
%}
%%
\n ln++;
\t bl=bl+5;
" " bl++;
[^ \t\n]+ {wr++;
         ch = ch+yyleng;
        }
%%

int main(int argc, char* argv[])
{
yyin = fopen(argv[1],"r");
yylex();
printf("Character=%d\nSpaces=%d\nLines=%d\nWords=%d", ch, bl, ln, wr);
return 0;
}

int yywrap()
{
return 1;
}

