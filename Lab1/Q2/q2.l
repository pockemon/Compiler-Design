%{
  #include <stdio.h>
  int pi=0,ni=0,pf=0,nf=0;
%}
%%
[-][0-9]+ {ni++;}
[+]?[0-9]+ {pi++;}
[-][0-9]*\.[0-9]+ {nf++;}
[+]?[0-9]*\.[0-9]+ {pf++;}
%%

void main(int argc,char *argv[])
{
 if (argc!=2)
 {
  printf("usage : ./a.out in.txt \n");
  exit(0);
 }
 yyin=fopen(argv[1],"r");
 yylex();
 printf("no. of positive integer %d \n",pi);
 printf("no. of negative integer %d \n",ni);
 printf("no. of positive fraction %d \n",pf);
 printf("no. of negative fraction %d \n",nf);
}

int yywrap()
{
 return 1;
}

