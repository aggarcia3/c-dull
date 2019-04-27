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
    : lista_declaraciones                                                     { printf ("  modulo -> list_decl%s", DELIM_SALTO_LINEA); }
    | lista_directivas_uso lista_declaraciones                                { printf ("  modulo -> lista_dir_uso list_decl%s", DELIM_SALTO_LINEA); }
    ;

lista_declaraciones
    : declaracion                                                             { printf ("  list_decl -> decl%s", DELIM_SALTO_LINEA); }
    | lista_declaraciones declaracion                                         { printf ("  list_decl -> list_decl decl%s", DELIM_SALTO_LINEA); }
    ;

declaracion
    : declaracion_espacio_nombres                                             { printf ("  decl -> decl_esp_nom%s", DELIM_SALTO_LINEA); }
    | declaracion_variable                                                    { printf ("  decl -> decl_var%s", DELIM_SALTO_LINEA); }
    | declaracion_tipo                                                        { printf ("  decl -> decl_tipo%s", DELIM_SALTO_LINEA); }
    | declaracion_funcion                                                     { printf ("  decl -> decl_func%s", DELIM_SALTO_LINEA); }
    ;

lista_directivas_uso
    : directiva_uso                                                           { printf ("  lista_dir_uso -> dir_uso%s", DELIM_SALTO_LINEA); }
    | lista_directivas_uso directiva_uso                                      { printf ("  lista_dir_uso -> lista_dir_uso dir_uso%s", DELIM_SALTO_LINEA); }
    ;

directiva_uso
    : USING IDENTIFICADOR '=' nombre_tipo_o_espacio_nombres ';'               { printf ("  dir_uso -> USING ID = nom_tipo_o_esp_noms%s", DELIM_SALTO_LINEA); }
    | USING nombre_tipo_o_espacio_nombres ';'                                 { printf ("  dir_uso -> nom_tipo_o_esp_noms%s", DELIM_SALTO_LINEA); }
    ;

nombre_tipo_o_espacio_nombres
    : identificador_con_tipos                                                 { printf ("  nom_tipo_o_esp_noms -> id_tipos%s", DELIM_SALTO_LINEA); }
    | nombre_tipo_o_espacio_nombres '.' identificador_con_tipos               { printf ("  nom_tipo_o_esp_noms -> nom_tipo_o_esp_noms . id_tipos%s", DELIM_SALTO_LINEA); }
    ;

identificador_con_tipos
    : IDENTIFICADOR                                                           { printf ("  id_tipos -> ID%s", DELIM_SALTO_LINEA); }
    | IDENTIFICADOR '(' lista_nombres_tipo_o_espacio_nombres ')'              { printf ("  id_tipos -> ID ( list_tipos )%s", DELIM_SALTO_LINEA); }
    ;

lista_nombres_tipo_o_espacio_nombres
    : nombre_tipo_o_espacio_nombres                                           { printf ("  list_nom_tipo_o_esp_noms -> nom_tipo_o_esp_noms%s", DELIM_SALTO_LINEA); }
    | lista_nombres_tipo_o_espacio_nombres ',' nombre_tipo_o_espacio_nombres  { printf ("  list_nom_tipo_o_esp_noms -> list_nom_tipo_o_esp_noms nom_tipo_o_esp_noms%s", DELIM_SALTO_LINEA); }
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
    : '{' lista_declaraciones '}'							{ printf("  blq_espacio_noms -> { list_decl }%s", DELIM_SALTO_LINEA); }
    | '{' lista_directivas_uso lista_declaraciones '}'					{ printf("  blq_espacio_noms -> { list_dir_uso list_decl }%s", DELIM_SALTO_LINEA); }
;

/*************/
/* VARIABLES */
/*************/


/*********/
/* TIPOS */
/*********/


/**********/
/* CLASES */
/**********/


/*************/
/* FUNCIONES */
/*************/


/*****************/
/* INSTRUCCIONES */
/*****************/


/***************/
/* EXPRESIONES */
/***************/


%%

 /* Código del usuario */

int yyerror(char* str) {
	fprintf(stderr, "Línea %u, token no reconocido: %s%s", numLinea, str, DELIM_SALTO_LINEA);
	codigoSalida = EXIT_FAILURE;
	return 0;
}

/*int yywrap() {
	return 1;
}*/

int main(int argc, char* argv[]) {
	FILE* fichFuente;

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
