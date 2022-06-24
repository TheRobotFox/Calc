%{

#include <stdio.h>
#include <string.h>
#include "List.h"
#include "Calc.h"
void showError();
void lexString();
void lexNormal();
%}

%option caseless
%x READSTRING

double	([0-9]+[.]?[0-9]*)
exit    (exit|quit|q)
name 	([a-z][a-z0-9]*)
str ([^\n]+)
%%

<READSTRING>{str} {
		size_t len = strlen(yytext)+1;
		yylval.str = malloc(len+1);
		memcpy(yylval.str, yytext, len);
		yylval.str[len-1]=';';
		yylval.str[len]=0;
		return (STRING);
}
{double}	{sscanf(yytext, "%lf", &yylval.numd); return (NUM);}
"pi"		{yylval.numd=3.1415926535897932384; return (NUM);}
"e"		{yylval.numd=2.71828182845904523536028747135266249775724709369995; return (NUM);}
"def" {return (DEF);}
"," 	{return (COMMA);}
"+"	    {return (ADD);}
"-"	    {return (SUB);}
"*"	    {return (MUL);}
"/"	    {return (DIV);}
"^"	    {return (POW);}
"!"     {return (FAC);}
"=="    {return (EQUALS);}
"="     {return (ASSIGN);}
"("	    {return (PO);}
")"	    {return (PC);}
";"     {return (SC);}
" "
"\n"	{return (BRK);}
{exit}  {return (EXIT);}
{name}	{
		size_t len = strlen(yytext)+1;
		yylval.str = malloc(len);
		memcpy(yylval.str, yytext, len);
		return (NAME);
	}
<READSTRING,INITIAL>.	{showError(); return (OTH);}

%%

void lexNormal(){BEGIN(INITIAL);}
void lexString(){BEGIN(READSTRING);}

void showError(){
    printf("Invalid Input!");
}

bool comp(void* a, void*b)
{
	return *(int*)a==*(int*)b;
}
int main(int argc, char** argv)
{
    	yyparse();

    	return 0;
}
