EJECUTABLE=C-Dull

$(EJECUTABLE): analizador_sintactico.tab.c lex.yy.c
	gcc -o "$@" -lm $^
analizador_sintactico.tab.c: analizador_sintactico.y
	bison -v "$<"
analizador_sintactico.tab.h: analizador_sintactico.y
	bison -d "$<"
lex.yy.c: analizador_lexico.l analizador_sintactico.tab.h
	flex "$<"
clean:
	rm -rf $(EJECUTABLE) analizador_sintactico.tab.c analizador_sintactico.tab.h lex.yy.c analizador_sintactico.output
test: $(EJECUTABLE)
	for fichero in `find tests -type f -iname '*.c*' -print`; do echo "-------------------------"; echo "Probando $(EJECUTABLE) con $$fichero..."; echo; "./$(EJECUTABLE)" "$$fichero"; echo; echo "-------------------------"; done
