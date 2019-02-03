sloth: lang.o
	gcc -o calc pars.tab.c lang.o

lang.o: lang.lex.c
	gcc -c lang.lex.c -o lang.o

lang.lex.c: lang.l pars.tab.c
	gcc -o lang.o lang.lex.c

lang.lex.c: lang.l
	flex -o lang.lex.c lang.l

pars.tab.c:
	bison -d pars.y

clean:
	rm calc lang.lex.* lang.o pars.tab.*
