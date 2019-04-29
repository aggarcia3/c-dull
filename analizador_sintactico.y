%{ /* Código C */
	#include <stdlib.h>
	#include <stdio.h>
	#include <math.h>
	#include <errno.h>

	/* Modo depuración de Bison (salida más verbosa) */
	#define YYDEBUG 1

	extern FILE *yyin;

	/* Los caracteres delimitadores de saltos de línea que se usarán para la salida de datos. En formato Unix, los saltos de línea se indican por el caracter Line Feed (\n) */
	#define DELIM_SALTO_LINEA "\n"

	/* El código de salida del programa a devolver al SO */
	int codigoSalida = EXIT_SUCCESS;

	/* El número de línea en el que se encuentra actualmente procesando la entrada el autómata */
	extern unsigned int numLinea;

	/* Declaraciones de funciones usadas, para evitar advertencias del compilador */
	int yylex(void);
	int yyerror(char*);
%}

 /* Declaraciones de BISON */
%token ABSTRACT AUTO BASE BOOLEAN BREAK CASE CATCH CHAR CLASS CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTERN
%token FINALLY FLOAT FOR GOTO IF INCLUDE INT INTERFACE INTERNAL LONG NAMESPACE NEW OVERRIDE PRIVATE PROTECTED PUBLIC RETURN
%token SEALED SHORT SIGNED STATIC STRUCT SWITCH THIS THROW TRY TYPEDEF UNION UNSIGNED USING VIRTUAL VOID WHILE

%token IDENTIFICADOR ENTERO CADENA REAL CARACTER BOOLEANO SIZEOF PTR_ACCESO INC DEC DESPI DESPD LE GE EQ NEQ AND
%token OR MULT_ASIG DIV_ASIG MOD_ASIG SUMA_ASIG RESTA_ASIG DESPI_ASIG DESPD_ASIG AND_ASIG XOR_ASIG OR_ASIG

%%

 /* Gramática */

/************/
/* PROGRAMA */
/************/

modulo
    : lista_declaraciones							{ printf ("  modulo -> list_decl%s", DELIM_SALTO_LINEA); }
    | lista_directivas_uso lista_declaraciones					{ printf ("  modulo -> lista_dir_uso list_decl%s", DELIM_SALTO_LINEA); }
    ;

lista_declaraciones
    : declaracion								{ printf ("  list_decl -> decl%s", DELIM_SALTO_LINEA); }
    | lista_declaraciones declaracion						{ printf ("  list_decl -> list_decl decl%s", DELIM_SALTO_LINEA); }
    ;

declaracion
    : declaracion_espacio_nombres						{ printf ("  decl -> decl_esp_nom%s", DELIM_SALTO_LINEA); }
    | declaracion_variable							{ printf ("  decl -> decl_var%s", DELIM_SALTO_LINEA); }
    | declaracion_tipo								{ printf ("  decl -> decl_tipo%s", DELIM_SALTO_LINEA); }
    | declaracion_funcion							{ printf ("  decl -> decl_func%s", DELIM_SALTO_LINEA); }
    ;

lista_directivas_uso
    : directiva_uso								{ printf ("  lista_dir_uso -> dir_uso%s", DELIM_SALTO_LINEA); }
    | lista_directivas_uso directiva_uso					{ printf ("  lista_dir_uso -> lista_dir_uso dir_uso%s", DELIM_SALTO_LINEA); }
    ;

directiva_uso
    : USING IDENTIFICADOR '=' nombre_tipo_o_espacio_nombres ';'			{ printf ("  dir_uso -> USING ID = nom_tipo_o_esp_noms%s", DELIM_SALTO_LINEA); }
    | USING nombre_tipo_o_espacio_nombres ';'					{ printf ("  dir_uso -> nom_tipo_o_esp_noms%s", DELIM_SALTO_LINEA); }
    ;

nombre_tipo_o_espacio_nombres
    : identificador_con_tipos							{ printf ("  nom_tipo_o_esp_noms -> id_tipos%s", DELIM_SALTO_LINEA); }
    | nombre_tipo_o_espacio_nombres '.' identificador_con_tipos			{ printf ("  nom_tipo_o_esp_noms -> nom_tipo_o_esp_noms . id_tipos%s", DELIM_SALTO_LINEA); }
    ;

identificador_con_tipos
    : IDENTIFICADOR								{ printf ("  id_tipos -> ID%s", DELIM_SALTO_LINEA); }
    | IDENTIFICADOR '(' lista_nombres_tipo_o_espacio_nombres ')'		{ printf ("  id_tipos -> ID ( list_tipos )%s", DELIM_SALTO_LINEA); }
    ;

lista_nombres_tipo_o_espacio_nombres
    : nombre_tipo_o_espacio_nombres						{ printf ("  list_nom_tipo_o_esp_noms -> nom_tipo_o_esp_noms%s", DELIM_SALTO_LINEA); }
    | lista_nombres_tipo_o_espacio_nombres ',' nombre_tipo_o_espacio_nombres	{ printf ("  list_nom_tipo_o_esp_noms -> list_nom_tipo_o_esp_noms nom_tipo_o_esp_noms%s", DELIM_SALTO_LINEA); }
    ;

/*******************/
/* ESPACIO NOMBRES */
/*******************/

declaracion_espacio_nombres: NAMESPACE identificador_anidado bloque_espacio_nombres	{ printf("  decl_esp_noms -> NAMESPACE id_anidado blq_espacio_noms%s", DELIM_SALTO_LINEA); }
;

identificador_anidado
    : IDENTIFICADOR									{ printf("  id_anidado -> ID%s", DELIM_SALTO_LINEA); }
    | lista_identificadores_anidados IDENTIFICADOR					{ printf("  id_anidado -> list_id_anidados ID%s", DELIM_SALTO_LINEA); }
;

lista_identificadores_anidados
    : IDENTIFICADOR '.'									{ printf("  list_id_anidados -> ID .%s", DELIM_SALTO_LINEA); }
    | lista_identificadores_anidados IDENTIFICADOR '.'					{ printf("  list_id_anidados -> list_id_anidados ID .%s", DELIM_SALTO_LINEA); }
;

bloque_espacio_nombres
    : '{' lista_declaraciones '}'							{ printf("  blq_esp_noms -> { list_decl }%s", DELIM_SALTO_LINEA); }
    | '{' lista_directivas_uso lista_declaraciones '}'					{ printf("  blq_esp_noms -> { list_dir_uso list_decl }%s", DELIM_SALTO_LINEA); }
;

/*************/
/* VARIABLES */
/*************/

 /* TODO: reemplazar con las reglas de la gramática reales */
declaracion_variable: 'v';


/*********/
/* TIPOS */
/*********/

 /* TODO: reemplazar con las reglas de la gramática reales */
nombre_tipo: 'n';

declaracion_tipo: 't';

/**********/
/* CLASES */
/**********/


/*************/
/* FUNCIONES */
/*************/

 /* TODO: reemplazar con las reglas de la gramática reales */
declaracion_funcion: 'f';


/*****************/
/* INSTRUCCIONES */
/*****************/


/***************/
/* EXPRESIONES */
/***************/

expresion_constante
    : ENTERO		{ printf("  expr_cte -> ENTERO%s", DELIM_SALTO_LINEA); }
    | REAL		{ printf("  expr_cte -> REAL%s", DELIM_SALTO_LINEA); }
    | CADENA		{ printf("  expr_cte -> CADENA%s", DELIM_SALTO_LINEA); }
    | CARACTER		{ printf("  expr_cte -> CARACTER%s", DELIM_SALTO_LINEA); }
    | BOOLEANO		{ printf("  expr_cte -> BOOLEANO%s", DELIM_SALTO_LINEA); }
;

expresion_parentesis: '(' expresion ')' { printf("  expr_parentesis -> ( expr )%s", DELIM_SALTO_LINEA); }
;

expresion_funcional
    : identificador_anidado '(' ')'			{ printf("  expr_func -> id_anidado ( )%s", DELIM_SALTO_LINEA); }
    | identificador_anidado '(' lista_expresiones ')'	{ printf("  expr_func -> id_anidado ( list_expr )%s", DELIM_SALTO_LINEA); }
;

lista_expresiones
    : expresion				{ printf("  list_expr -> expr%s", DELIM_SALTO_LINEA); }
    | lista_expresiones expresion	{ printf("  list_expr -> lista_expr expr%s", DELIM_SALTO_LINEA); }
;

expresion_creacion_objeto
    : NEW identificador_anidado '(' ')'				{ printf("  expr_creacion_obj -> NEW id_anidado ( )%s", DELIM_SALTO_LINEA); }
    | NEW identificador_anidado '(' lista_expresiones ')'	{ printf("  expr_creacion_obj -> NEW id_anidado ( list_expr )%s", DELIM_SALTO_LINEA); }
;

expresion_indexada
    : identificador_anidado					{ printf("  expr_index -> id_anidado%s", DELIM_SALTO_LINEA); }
    | expresion_indexada '[' expresion ']'			{ printf("  expr_index -> expr_index [ expr ]%s", DELIM_SALTO_LINEA); }
    | expresion_indexada PTR_ACCESO identificador_anidado	{ printf("  expr_index -> expr_index -> id_anidado%s", DELIM_SALTO_LINEA); }
;

expresion_postfija
    : expresion_constante		{ printf("  expr_post -> expr_cte%s", DELIM_SALTO_LINEA); }
    | expresion_parentesis		{ printf("  expr_post -> expr_parentesis%s", DELIM_SALTO_LINEA); }
    | expresion_funcional		{ printf("  expr_post -> expr_func%s", DELIM_SALTO_LINEA); }
    | expresion_creacion_objeto		{ printf("  expr_post -> expr_creacion_obj%s", DELIM_SALTO_LINEA); }
    | expresion_indexada		{ printf("  expr_post -> expr_index%s", DELIM_SALTO_LINEA); }
    | expresion_postfija INC		{ printf("  expr_post -> expr_post INC%s", DELIM_SALTO_LINEA); }
    | expresion_postfija DEC		{ printf("  expr_post -> expr_post DEC%s", DELIM_SALTO_LINEA); }
;

expresion_prefija
    : expresion_postfija		{ printf("  expr_pre -> expr_post%s", DELIM_SALTO_LINEA); }
    | SIZEOF expresion_prefija		{ printf("  expr_pre -> SIZEOF expr_pre%s", DELIM_SALTO_LINEA); }
    | SIZEOF '(' nombre_tipo ')'	{ printf("  expr_pre -> SIZEOF ( nombre_tipo )%s", DELIM_SALTO_LINEA); }
    | operador_prefijo expresion_cast	{ printf("  expr_pre -> op_prefijo expr_cast%s", DELIM_SALTO_LINEA); }
;

operador_prefijo
    : INC	{ printf("  op_pre -> INC%s", DELIM_SALTO_LINEA); }
    | DEC	{ printf("  op_pre -> DEC%s", DELIM_SALTO_LINEA); }
    | '&'	{ printf("  op_pre -> &%s", DELIM_SALTO_LINEA); }
    | '*'	{ printf("  op_pre -> *%s", DELIM_SALTO_LINEA); }
    | '+'	{ printf("  op_pre -> +%s", DELIM_SALTO_LINEA); }
    | '-'	{ printf("  op_pre -> -%s", DELIM_SALTO_LINEA); }
    | '~'	{ printf("  op_pre -> ~%s", DELIM_SALTO_LINEA); }
    | '!'	{ printf("  op_pre -> !%s", DELIM_SALTO_LINEA); }
;

expresion_cast
    : expresion_prefija				{ printf("  expr_cast -> expr_pre%s", DELIM_SALTO_LINEA); }
    | '(' nombre_tipo ')' expresion_prefija	{ printf("  expr_cast -> ( nombre_tipo ) expr_pre%s", DELIM_SALTO_LINEA); }
;

expresion_or_logico
    : expresion_or_logico OR expresion_and_logico	{ printf("  expr_or_logico -> expr_or_logico OR expr_and_logico%s", DELIM_SALTO_LINEA); }
    | expresion_and_logico				{ printf("  expr_or_logico -> expr_and_logico%s", DELIM_SALTO_LINEA); }
;

expresion_and_logico
    : expresion_and_logico AND expresion_comparacion_igualdad	{ printf("  expr_and_logico -> expr_and_logico AND expr_comp_igualdad%s", DELIM_SALTO_LINEA); }
    | expresion_comparacion_igualdad				{ printf("  expr_and_logico -> expr_comp_igualdad%s", DELIM_SALTO_LINEA); }
;

expresion_comparacion_igualdad
    : expresion_comparacion_igualdad EQ expresion_comparacion_desigualdad	{ printf("  expr_comp_igualdad -> expr_comp_igualdad EQ expr_comp_desigualdad%s", DELIM_SALTO_LINEA); }
    | expresion_comparacion_igualdad NEQ expresion_comparacion_desigualdad	{ printf("  expr_comp_igualdad -> expr_comp_igualdad NEQ expr_comp_desigualdad%s", DELIM_SALTO_LINEA); }
    | expresion_comparacion_desigualdad						{ printf("  expr_comp_igualdad -> expr_comp_desigualdad%s", DELIM_SALTO_LINEA); }
;

expresion_comparacion_desigualdad
    : expresion_comparacion_desigualdad '<' expresion_or_binario	{ printf("  expr_comp_desigualdad -> expr_comp_desigualdad < expr_or_binario%s", DELIM_SALTO_LINEA); }
    | expresion_comparacion_desigualdad '>' expresion_or_binario	{ printf("  expr_comp_desigualdad -> expr_comp_desigualdad > expr_or_binario%s", DELIM_SALTO_LINEA); }
    | expresion_comparacion_desigualdad LE expresion_or_binario		{ printf("  expr_comp_desigualdad -> expr_comp_desigualdad <= expr_or_binario%s", DELIM_SALTO_LINEA); }
    | expresion_comparacion_desigualdad GE expresion_or_binario		{ printf("  expr_comp_desigualdad -> expr_comp_desigualdad >= expr_or_binario%s", DELIM_SALTO_LINEA); }
    | expresion_or_binario						{ printf("  expr_comp_desigualdad -> expr_or_binario%s", DELIM_SALTO_LINEA); }
;

expresion_or_binario
    : expresion_or_binario '|' expresion_xor_binario	{ printf("  expr_or_binario -> expr_or_binario | expr_xor_binario%s", DELIM_SALTO_LINEA); }
    | expresion_xor_binario				{ printf("  expr_or_binario -> expr_xor_binario%s", DELIM_SALTO_LINEA); }
;

expresion_xor_binario
    : expresion_xor_binario '^' expresion_and_binario	{ printf("  expr_xor_binario -> expr_xor_binario ^ expr_and_binario%s", DELIM_SALTO_LINEA); }
    | expresion_and_binario				{ printf("  expr_xor_binario -> expr_and_binario%s", DELIM_SALTO_LINEA); }
;

expresion_and_binario
    : expresion_and_binario '&' expresion_desplazamiento	{ printf("  expr_and_binario -> expr_and_binario & expr_despl%s", DELIM_SALTO_LINEA); }
    | expresion_desplazamiento					{ printf("  expr_and_binario -> expr_despl%s", DELIM_SALTO_LINEA); }
;

expresion_desplazamiento
    : expresion_desplazamiento DESPI expresion_suma	{ printf("  expr_despl -> expr_despl DESPI expr_suma%s", DELIM_SALTO_LINEA); }
    | expresion_desplazamiento DESPD expresion_suma	{ printf("  expr_despl -> expr_despl DESPD expr_suma%s", DELIM_SALTO_LINEA); }
    | expresion_suma					{ printf("  expr_despl -> expr_suma%s", DELIM_SALTO_LINEA); }
;

expresion_suma
    : expresion_suma '+' expresion_multiplicacion	{ printf("  expr_suma -> expr_suma + expr_mult%s", DELIM_SALTO_LINEA); }
    | expresion_suma '-' expresion_multiplicacion	{ printf("  expr_suma -> expr_suma - expr_mult%s", DELIM_SALTO_LINEA); }
    | expresion_multiplicacion				{ printf("  expr_suma -> expr_mult%s", DELIM_SALTO_LINEA); }
;

expresion_multiplicacion
    : expresion_multiplicacion '*' expresion_logica	{ printf("  expr_mult -> expr_mult * expr_logica%s", DELIM_SALTO_LINEA); }
    | expresion_multiplicacion '/' expresion_logica	{ printf("  expr_mult -> expr_mult / expr_logica%s", DELIM_SALTO_LINEA); }
    | expresion_multiplicacion '%' expresion_logica	{ printf("  expr_mult -> expr_mult % expr_logica%s", DELIM_SALTO_LINEA); }
    | expresion_logica					{ printf("  expr_mult -> expr_logica%s", DELIM_SALTO_LINEA); }
    | expresion_multiplicacion '*' expresion_cast	{ printf("  expr_mult -> expr_mult * expr_cast%s", DELIM_SALTO_LINEA); }
    | expresion_multiplicacion '/' expresion_cast	{ printf("  expr_mult -> expr_mult / expr_cast%s", DELIM_SALTO_LINEA); }
    | expresion_multiplicacion '%' expresion_cast	{ printf("  expr_mult -> expr_mult % expr_cast%s", DELIM_SALTO_LINEA); }
    | expresion_cast					{ printf("  expr_mult -> expr_cast%s", DELIM_SALTO_LINEA); }
;

expresion_logica: expresion_or_logico { printf("  expr_logica -> expr_or_logico%s", DELIM_SALTO_LINEA); }
;

expresion
    : expresion_logica					{ printf("  expr -> expr_logica%s", DELIM_SALTO_LINEA); }
    | expresion_logica '?' expresion ':' expresion	{ printf("  expr -> expr_logica ? expr : expr%s", DELIM_SALTO_LINEA); }
;

%%

 /* Código del usuario */

int yyerror(char* str) {
	fprintf(stderr, "Línea %u, token no reconocido: %s%s", numLinea, str, DELIM_SALTO_LINEA);
	codigoSalida = EXIT_FAILURE;
	return 0;
}

int main(int argc, char* argv[]) {
	FILE* fichFuente;

	yydebug = 1;

	/* Solo podemos abrir un fichero si tenemos una ruta hacia él */
	if (argc < 2) {
		printf("Sintaxis: %s [ruta al fichero de código fuente C-Dull]%s", argv[0], DELIM_SALTO_LINEA);
	} else {
		fichFuente = fopen(argv[1], "r");
		/* Si todo fue bien abriendo el fichero, asociarlo a la entrada del autómata generado por Flex y comenzar el reconocimiento de tokens */
		if (fichFuente) {
			yyin = fichFuente;

			yyparse();

			fclose(fichFuente);
		} else {
			codigoSalida = errno;
			perror("Ha ocurrido un error de E/S al abrir el fichero de código fuente");
		}
	}

	return codigoSalida;
}
