default:
	clear
	flex --header-file=lex.h -I Calc.l 
	bison -dv Calc.y 
	gcc -o Calc Calc.tab.c lex.yy.c List.c -lfl -lm -s -O3
