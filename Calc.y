%{
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "List.h"
#include "lex.h"
void lexString();
void lexNormal();
int yyerror(const char *s);
int yylex();
void error(const char*e);
struct Variable *getVar(char *name);

struct Func_search
{
	char *name;
	size_t args;
};
struct Variable
{
	char *name;
	double val;
};
struct Func
{
	char *name;
	List *args;
	char *val;
};

static bool comp_func(void *f, void *name);
static bool comp_vars(void *v, void *name);
void print(void *a);
double general_log(double base, double val);
double root(double a, double b);
struct {
	const char *name;
	double (*func)(double var);
} f_unary[] = {
	{"ln", log},
	{"sin", sin},
	{"cos", cos},
	{"root", sqrt},
};
struct {
	char *name;
	double (*func)(double var2, double var);
} f_binary[] = {
	{"log",general_log},
	{"root", root}
};
List *vars_global;
List *argstack;
List *functions;

List *argnames;

char eval_buf[512];
%}
%define parse.error detailed
%glr-parser
//%expect 5
//%expect-rr 1
%token NUM BRK ADD SUB MUL DIV PO PC OTH POW COMMA FAC EQUALS EXIT NAME STRING SC ASSIGN DEF

%union{
	double numd;
	char *str;
	List *l;
}

%type<numd> EXPR5 EXPR4 EXPR3 EXPR2 EXPR NUM EQUASION ARG_V TERM EXPR6
%type<str>  NAME STRING ARG_N
%type<l> ARGLIST_V ARGLIST_N

%%

prog:
{
	 	puts("Calc 0.3");
		vars_global=List_create(sizeof(struct Variable));
		argstack = List_create(sizeof(List*));
		functions = List_create(sizeof(struct Func));
		argnames = List_create(sizeof(char*));
	 }
	 OPERATIONS
	 {
		List_free(vars_global);
		List_free(argstack);
		List_free(functions);
		List_free(argnames);
	 	puts("Byebye!");
		exit(0);
	 }

OPERATIONS:
 	EXIT BRK | BRK OPERATIONS | OPERATION BRK OPERATIONS

OPERATION:
	EXPR {
		printf("%lg\n",$1);
	}
	| EQUASION {
		puts(isnan($1) ? "false" : "true");
	}
	| DEFINITION
DEFINITION:
		DEFVAR
		| DEF DEFFUNC

DEFVAR:
		NAME ASSIGN EXPR{
				struct Variable *vp = List_find(vars_global, comp_vars, $1);
				if(vp)
					vp->val=$3;
				else{
				struct Variable v;
				v.name=$1;
				v.val=$3;
				List_append(vars_global,&v);
				}
				}
DEFFUNC:
			 NAME ARGLIST_N ASSIGN{
			 lexString();
			 }STRING{
			 lexNormal();
			 struct Func f;
			 f.name=$1;
			 f.val=$5;
			 f.args=argnames;
			 argnames=List_create(sizeof(char*));
			List_append(functions,&f);
			}

EQUASION:
	TERM EQUALS TERM{
		$$ = fabs($1-$3)<0.000001 ? $1 : NAN;
		}
	| EQUASION EQUALS TERM{
		$$ = fabs($1-$3)<0.000001 ? $1 : NAN;
	}

TERM:
		EXPR

EXPR:
	EXPR ADD EXPR2 %dprec 1{
		$$ = $1 + $3;
	}
	| EXPR SUB EXPR2 %dprec 3{
		$$ = $1 - $3;
	}
	| EXPR2 %dprec 2

EXPR2:
	EXPR3 %dprec 2
//	| EXPR2 EXPR3 %dprec 1 {
//		$$ = $1 * $2;
//	}
	| EXPR2 MUL EXPR3 {
		$$ = $1 * $3;
	}
	| EXPR2 DIV EXPR3 {
		$$ = $1 / $3;
	}
	| SUB EXPR3 {
		$$=-$2;
	}

EXPR3:
	EXPR4 %dprec 1
	| EXPR4 POW EXPR3 %dprec 2{
		$$ = pow($1,$3);
	}
	| NAME ARGLIST_V {
		char* str;
		struct Func_search tmp = {$1,List_size($2)};
		struct Func *f = List_find(functions,comp_func,&tmp);
		List *locals=List_create(sizeof(struct Variable));
		List_append(argstack, &locals);
		if(f){


			struct Variable tmp;
			for(size_t i=0; i< List_size(f->args); i++)
			{
					tmp.val=*(double*)List_get($2,i);
					tmp.name=List_get(f->args,i);
					List_append(locals, &tmp);
			}
			str=f->val;
		}else{
			double res;
			switch(List_size($2))
			{
			case 1:{
				double *var=List_get($2,0);
				for(size_t i; i<sizeof(f_unary)/sizeof(*f_unary); i++)
				{
					if(!strcmp($1,f_unary[i].name)){
						res=f_unary[i].func(*var);
						break;
					}
				}
				break;
				}
			case 2: {
				double *var1=List_get($2,0), *var2=List_get($2,1);
				for(size_t i; i<sizeof(f_binary)/sizeof(*f_binary); i++)
				{
					if(!strcmp($1,f_binary[i].name)){
						res=f_binary[i].func(*var1,*var2);
						break;
					}
				}
				break;
				}
			default:
				error("Unknown function");
				break;
			}
			snprintf(eval_buf,512,"%20lf;",res);
			str=eval_buf;
		}
		FILE* tmpfile=  fopen("TMP","r");
		YY_BUFFER_STATE tmp_buf = yy_create_buffer(tmpfile,YY_BUF_SIZE);
		yypush_buffer_state(tmp_buf);
		YY_BUFFER_STATE buff = yy_scan_string(str);
		yy_delete_buffer(tmp_buf);
	 } EXPR SC{
		$$=$4;
		printf("%s->%lg\n", $1, $$);
		List_free(*(List**)List_pop(argstack));
		yypop_buffer_state();
	}

EXPR4:
     	EXPR5
	| EXPR4 FAC {
		$$ = tgamma($1+1);
	}
EXPR5:
	EXPR6 | PO EXPR PC {
		$$ = $2;
	}

EXPR6:
	NUM{
		$$=$1;
	}
	| NAME{
		struct Variable *val = getVar($1);
		free($1);
		if(!val)
			error("Unknown Variable");

		$$ = val->val;
	}
ARGLIST_V:
  PO{
	List *l = List_create(sizeof(struct Variable));
	List_push(argstack,l);
	} ARGS_V PC{
		$$=*(List**)List_get(argstack,List_size(argstack)-1);
	}
	| PO PC{
		$$=*(List**)List_get(argstack,List_size(argstack)-1);
	}
	| EXPR6{
	$$=*(List**)List_get(argstack,List_size(argstack)-1);
	List_append($$, &$1);
	}
ARGS_V:
    ARG_V	| ARGS_V COMMA ARG_V

ARG_V:
   EXPR{
        List_append(arglist, &$1);
	 }
ARGLIST_N:
		PO{
		List_clear(argnames);
		} ARGS_N PC{
			$$=argnames;
		}
		| PO PC {
			List_clear(argnames);
			$$=argnames;
		}
ARGS_N:
			ARG_N
			| ARGS_N COMMA ARG_N
ARG_N:
		 NAME{
		 	List_append(argnames, $1);
			}
%%


double general_log(double base, double val)
{
	return log(val)/log(base);
}
double root(double a, double b)
{
	return pow(a,1.0/b);
}

static bool comp_func(void *f, void *name)
{
	struct Func *_f=f;
	struct Func_search *_name=name;
	return !(strcmp(_f->name,_name->name)&&(List_size(_f->args)!=_name->args));
}
static bool comp_vars(void *v, void *name)
{
	struct Variable * var=v;
	const char* str = name;
	return !strcmp(var->name,str);
}

struct Variable *getVar(char *name)
{
	//List_print(vars_global, print);
	printf("%d\n", List_size(argstack));
	struct Variable *var=0;
	if(List_size(argstack)){
	List **locals=List_get(argstack,List_size(argstack)-1);
	var = List_find(*locals, comp_vars, name);
	}
	if(!var)
		var = List_find(vars_global, comp_vars, name);
	return var;
}

void print(void *a)
{
	struct Variable *v =a;
	printf("<%s,%d>",v->name, v->val);
}
int yyerror(const char *s)
{
	printf("Syntax Error on line %s\n", s);
	return 1;
}
void error(const char*e)
{
	printf("Error: %s",e);
	exit(0);
}
