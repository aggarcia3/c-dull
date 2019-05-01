%{ /* Código C */
	#include <stdlib.h>
	#include <stdio.h>
	#include <errno.h>

	/* Si está definido, la salida mostrará información verbosa acerca del funcionamiento interno del analizador */
	#define SALIDA_VERBOSA

#ifdef SALIDA_VERBOSA
	/* Permite utilizar el modo de depuración de Bison. No lo activa por sí mismo */
	#define YYDEBUG 1
#endif

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

declaracion_variable: tipo lista_nombres ';'    { printf("  decl_var -> tipo list_nom ;%s", DELIM_SALTO_LINEA); }
;

lista_nombres
    : nombre    { printf("  list_nom -> nom%s", DELIM_SALTO_LINEA); }
    | lista_nombres nombre    { printf("  list_nom -> list_nom nom%s", DELIM_SALTO_LINEA); }
;

tipo
    : '<' nombre_tipo_o_espacio_nombres '>'    { printf("  tipo -> < nom_tipo_o_esp_noms >%s", DELIM_SALTO_LINEA); }
    | tipo_escalar    { printf("  tipo -> tipo_esc%s", DELIM_SALTO_LINEA); }
;

tipo_escalar
    : tipo_basico    { printf("  tipo_esc -> tipo_basic%s", DELIM_SALTO_LINEA); }
    | signo tipo_basico    { printf("  tipo_esc -> signo tipo_basic%s", DELIM_SALTO_LINEA); }
    | longitud tipo_basico    { printf("  tipo_esc -> long tipo_basic%s", DELIM_SALTO_LINEA); }
    | signo longitud tipo_basico    { printf("  tipo_esc -> signo long tipo_basic%s", DELIM_SALTO_LINEA); }
;

longitud
    : SHORT    { printf("  long -> SHORT%s", DELIM_SALTO_LINEA); }
    | LONG    { printf("  long -> LONG%s", DELIM_SALTO_LINEA); }
;

signo
    : SIGNED    { printf("  signo -> SIGNED%s", DELIM_SALTO_LINEA); }
    | UNSIGNED    { printf("  signo -> UNSIGNED%s", DELIM_SALTO_LINEA); }
;

tipo_basico
    : CHAR    { printf("  tipo_basic -> CHAR%s", DELIM_SALTO_LINEA); }
    | INT    { printf("  tipo_basic -> INT%s", DELIM_SALTO_LINEA); }
    | FLOAT    { printf("  tipo_basic -> FLOAT%s", DELIM_SALTO_LINEA); }
    | DOUBLE    { printf("  tipo_basic -> DOUBLE%s", DELIM_SALTO_LINEA); }
    | BOOLEAN    { printf("  tipo_basic -> BOOLEAN%s", DELIM_SALTO_LINEA); }
;

nombre
    : dato    { printf("  nom -> dato%s", DELIM_SALTO_LINEA); }
    | dato '=' valor    { printf("  nom -> dato = valor%s", DELIM_SALTO_LINEA); }
;

dato
    : dato_indexado    { printf("  dato -> dato_index%s", DELIM_SALTO_LINEA); }
    | lista_operadores_puntero dato_indexado    { printf("  dato -> list_op_punt dato_index%s", DELIM_SALTO_LINEA); }
;

dato_indexado
    : IDENTIFICADOR    { printf("  dato_index -> ID%s", DELIM_SALTO_LINEA); }
    | IDENTIFICADOR '[' lista_expresiones ']'    { printf("  dato_index -> ID [ list_expr ]%s", DELIM_SALTO_LINEA); }
;

valor
    : expresion    { printf("  valor -> expr%s", DELIM_SALTO_LINEA); }
    | '{' lista_valores '}'    { printf("  valor -> list_val%s", DELIM_SALTO_LINEA); }
;

lista_valores
    : valor    { printf("  list_val -> val%s", DELIM_SALTO_LINEA); }
    | lista_valores valor    { printf("  list_val -> list_val valor%s", DELIM_SALTO_LINEA); }
;
    
/*********/
/* TIPOS */
/*********/

declaracion_tipo
    : nombramiento_tipo    { printf("  decl_tipo -> nombram_tipo%s", DELIM_SALTO_LINEA); }
    | declaracion_struct_union    { printf("  decl_tipo -> decl_struct_union%s", DELIM_SALTO_LINEA); }
    | declaracion_interfaz    { printf("  decl_tipo -> decl_interf%s", DELIM_SALTO_LINEA); }
    | declaracion_enum    { printf("  decl_tipo -> decl_enum%s", DELIM_SALTO_LINEA); }
    | declaracion_clase    { printf("  decl_tipo -> decl_clase%s", DELIM_SALTO_LINEA); }
;

nombramiento_tipo : TYPEDEF tipo IDENTIFICADOR ';'    { printf("  nombram_tipo -> TYPEDEF tipo ID ;%s", DELIM_SALTO_LINEA); }
;

declaracion_struct_union
    : struct_union '{' lista_declaraciones_campo '}'    { printf("  decl_struct_union -> struct_union { list_decl_campo }%s", DELIM_SALTO_LINEA); }
    | lista_modificadores struct_union '{' lista_declaraciones_campo '}'    { printf("  decl_struct_union -> list_modif struct_union { list_decl_campo }%s", DELIM_SALTO_LINEA); }
    | struct_union IDENTIFICADOR '{' lista_declaraciones_campo '}'    { printf("  decl_struct_union -> struct_union ID { list_decl_campo }%s", DELIM_SALTO_LINEA); }
    | lista_modificadores struct_union IDENTIFICADOR '{' lista_declaraciones_campo '}'    { printf("  decl_struct_union -> list_modif struct_union ID { list_decl_campo }%s", DELIM_SALTO_LINEA); }
;

lista_declaraciones_campo    
    : declaracion_campo    { printf("  list_decl_campo -> decl_campo%s", DELIM_SALTO_LINEA); }
    | lista_declaraciones_campo declaracion_campo    { printf("  list_decl_campo -> list_decl_campo decl_campo%s", DELIM_SALTO_LINEA); }
;

lista_modificadores
    : modificador    { printf("  list_modif -> modif%s", DELIM_SALTO_LINEA); }
    | lista_modificadores modificador    { printf("  list_modif -> list_modif modif%s", DELIM_SALTO_LINEA); }
;

modificador
    : NEW    { printf("  modif -> NEW%s", DELIM_SALTO_LINEA); }
    | PUBLIC    { printf("  modif -> PUBLIC%s", DELIM_SALTO_LINEA); }
    | PROTECTED    { printf("  modif -> PROTECTED%s", DELIM_SALTO_LINEA); }
    | INTERNAL    { printf("  modif -> INTERNAL%s", DELIM_SALTO_LINEA); }
    | PRIVATE    { printf("  modif -> PRIVATE%s", DELIM_SALTO_LINEA); }
    | STATIC    { printf("  modif -> STATIC%s", DELIM_SALTO_LINEA); }
    | VIRTUAL    { printf("  modif -> VIRTUAL%s", DELIM_SALTO_LINEA); }
    | SEALED    { printf("  modif -> SEALED%s", DELIM_SALTO_LINEA); }
    | OVERRIDE    { printf("  modif -> OVERRIDE%s", DELIM_SALTO_LINEA); }
    | ABSTRACT    { printf("  modif -> ABSTRACT%s", DELIM_SALTO_LINEA); }
    | EXTERN    { printf("  modif -> EXTERN%s", DELIM_SALTO_LINEA); }
;

struct_union
    : STRUCT    { printf("  struct_union -> STRUCT%s", DELIM_SALTO_LINEA); }
    | UNION    { printf("  struct_union -> UNION%s", DELIM_SALTO_LINEA); }
;

declaracion_campo
    : tipo lista_nombres ';'    { printf("  decl_campo -> tipo list_nom%s ;", DELIM_SALTO_LINEA); }
    | declaracion_struct_union lista_nombres ';'    { printf("  decl_campo -> decl_struct_union list_nom%s ;", DELIM_SALTO_LINEA); }
;

declaracion_interfaz
    : INTERFACE IDENTIFICADOR herencia cuerpo_interfaz    { printf("  decl_interf -> INTERFACE ID herencia cuerpo_interf%s", DELIM_SALTO_LINEA); }
    | lista_modificadores INTERFACE IDENTIFICADOR herencia cuerpo_interfaz    { printf("  decl_interf -> list_modif INTERFACE ID herencia cuerpo_interf%s", DELIM_SALTO_LINEA); }
;

herencia
    :    { printf("  herencia -> %s", DELIM_SALTO_LINEA); }
    | ';' lista_nombres_tipo_o_espacio_nombres    { printf("  herencia -> ; list_nom_tipo_o_esp_noms%s", DELIM_SALTO_LINEA); }
;

cuerpo_interfaz
    : '{' '}' ';'    { printf("  cuerpo_interf -> { } ;%s", DELIM_SALTO_LINEA); }
    | '{' lista_declaracion_metodo_interfaz '}'    { printf("  cuerpo_interf -> { list_decl_met_interf } ;%s", DELIM_SALTO_LINEA); }
;

lista_declaracion_metodo_interfaz
    : declaracion_metodo_interfaz    { printf("  list_decl_met_interf -> decl_met_interf%s", DELIM_SALTO_LINEA); }
    | lista_declaracion_metodo_interfaz declaracion_metodo_interfaz    { printf("  list_decl_met_interf -> list_decl_met_interf decl_met_interf%s", DELIM_SALTO_LINEA); }
;

declaracion_metodo_interfaz
    : firma_funcion ';'    { printf("  decl_met_interf -> firma_func ;%s", DELIM_SALTO_LINEA); }
    | NEW firma_funcion ';'    { printf("  decl_met_interf -> NEW firma_func ;%s", DELIM_SALTO_LINEA); }
;

declaracion_enum
    : ENUM IDENTIFICADOR cuerpo_enum    { printf("  decl_enum -> ENUM ID cuerpo_enum%s", DELIM_SALTO_LINEA); }
    | lista_modificadores ENUM IDENTIFICADOR cuerpo_enum    { printf("  decl_enum -> list_modif ENUM ID cuerpo_enum%s", DELIM_SALTO_LINEA); }
    | ENUM IDENTIFICADOR ':' tipo_escalar cuerpo_enum    { printf("  decl_enum -> ENUM ID : tipo_esc cuerpo_enum%s", DELIM_SALTO_LINEA); }
    | lista_modificadores ENUM IDENTIFICADOR ':' tipo_escalar cuerpo_enum    { printf("  decl_enum -> list_modif ENUM ID : tipo_esc cuerpo_enum%s", DELIM_SALTO_LINEA); }
;

cuerpo_enum: '{' lista_declaraciones_miembro_enum '}'    { printf("  cuerpo_enum -> { list_decl_miembro_enum }%s", DELIM_SALTO_LINEA); }
;

lista_declaraciones_miembro_enum
    : declaracion_miembro_enum    { printf("  list_decl_miembro_enum -> decl_miembro_enum%s", DELIM_SALTO_LINEA); }
    | lista_declaraciones_miembro_enum declaracion_miembro_enum    { printf("  list_decl_miembro_enum -> list_decl_miembro_enum decl_miembro_enum%s", DELIM_SALTO_LINEA); }
;

declaracion_miembro_enum
    : IDENTIFICADOR    { printf("  decl_miembro_enum -> ID%s", DELIM_SALTO_LINEA); }
    | IDENTIFICADOR '=' expresion    { printf("  decl_miembro_enum -> ID = expr%s", DELIM_SALTO_LINEA); }
;

/**********/
/* CLASES */
/**********/

declaracion_clase
    : CLASS IDENTIFICADOR herencia cuerpo_clase    { printf("  decl_clase -> CLASS ID herencia cuerpo_clase%s", DELIM_SALTO_LINEA); }
    | lista_modificadores CLASS IDENTIFICADOR herencia cuerpo_clase    { printf("  decl_clase -> list_modif CLASS ID herencia cuerpo_clase%s", DELIM_SALTO_LINEA); }
;

cuerpo_clase: '{' lista_declaracion_elemento_clase '}'    { printf("  cuerpo_clase -> { list_decl_elem_clase }%s", DELIM_SALTO_LINEA); }
;

lista_declaracion_elemento_clase
    : declaracion_elemento_clase    { printf("  list_decl_elem_clase -> decl_elem_clase%s", DELIM_SALTO_LINEA); }
    | lista_declaracion_elemento_clase declaracion_elemento_clase    { printf("  list_decl_elem_clase -> list_decl_elem_clase decl_elem_clase%s", DELIM_SALTO_LINEA); }
;

declaracion_elemento_clase
    : declaracion_tipo    { printf("  decl_elem_clase -> decl_tipo%s", DELIM_SALTO_LINEA); }
    | declaracion_atributo    { printf("  decl_elem_clase -> decl_atrib%s", DELIM_SALTO_LINEA); }
    | declaracion_metodo    { printf("  decl_elem_clase -> decl_metodo%s", DELIM_SALTO_LINEA); }
    | declaracion_constructor    { printf("  decl_elem_clase -> decl_construct%s", DELIM_SALTO_LINEA); }
    | declaracion_destructor    { printf("  decl_elem_clase -> decl_destruct%s", DELIM_SALTO_LINEA); }
;

declaracion_atributo
    : declaracion_variable    { printf("  decl_atrib -> decl_var%s", DELIM_SALTO_LINEA); }
    | lista_modificadores declaracion_variable    { printf("  decl_atrib -> list_modif decl_var%s", DELIM_SALTO_LINEA); }
;

declaracion_metodo
    : firma_funcion bloque_instrucciones    { printf("  decl_metodo -> firma_func bloque_instr%s", DELIM_SALTO_LINEA); }
    | lista_modificadores firma_funcion bloque_instrucciones    { printf("  decl_metodo -> list_modif firma_func bloque_instr%s", DELIM_SALTO_LINEA); }
;

declaracion_constructor
    : cabecera_constructor bloque_instrucciones    { printf("  decl_construct -> cabec_construct bloque_instr%s", DELIM_SALTO_LINEA); }
    | lista_modificadores cabecera_constructor bloque_instrucciones    { printf("  decl_construct -> list_modif cabec_construct bloque_instr%s", DELIM_SALTO_LINEA); }
;

cabecera_constructor
    : IDENTIFICADOR parametros    { printf("  cabec_construct -> ID params%s", DELIM_SALTO_LINEA); }
    | IDENTIFICADOR parametros inicializador_constructor    { printf("  cabec_construct -> ID params inicial_construct%s", DELIM_SALTO_LINEA); }
;

inicializador_constructor
    : ':' BASE parametros    { printf("  inicial_construct -> : BASE params%s", DELIM_SALTO_LINEA); }
    | ':' THIS parametros    { printf("  inicial_construct -> : THIS params%s", DELIM_SALTO_LINEA); }
;

declaracion_destructor
    : cabecera_destructor bloque_instrucciones    { printf("  decl_construct -> cabec_destruct bloque_instr%s", DELIM_SALTO_LINEA); }
    | lista_modificadores cabecera_destructor bloque_instrucciones    { printf("  decl_construct -> list_modif cabec_destruct bloque_instr%s", DELIM_SALTO_LINEA); }
;

cabecera_destructor: '~' IDENTIFICADOR '(' ')'    { printf("  cabec_destruct -> : ~ ID ( )%s", DELIM_SALTO_LINEA); }
;

/*************/
/* FUNCIONES */
/*************/

declaracion_funcion: firma_funcion bloque_instrucciones { printf("  decl_func -> firma_func blq_instr%s", DELIM_SALTO_LINEA); }
;

firma_funcion
    : VOID IDENTIFICADOR parametros				{ printf("  firma_func -> VOID ID params%s", DELIM_SALTO_LINEA); }
    | tipo IDENTIFICADOR parametros				{ printf("  firma_func -> tipo ID params%s", DELIM_SALTO_LINEA); }
    | tipo lista_operadores_puntero IDENTIFICADOR parametros	{ printf("  firma_func -> tipo list_op_puntero ID params%s", DELIM_SALTO_LINEA); }
;

lista_operadores_puntero
    : '*'				{ printf("  list_op_punt -> *%s", DELIM_SALTO_LINEA); }
    | lista_operadores_puntero '*'	{ printf("  list_op_punt -> list_op_punt *%s", DELIM_SALTO_LINEA); }
;

parametros
    : '(' ')'					{ printf("  params -> ( )%s", DELIM_SALTO_LINEA); }
    | '(' argumentos ')'			{ printf("  params -> ( args )%s", DELIM_SALTO_LINEA); }
    | '(' lista_argumentos ')'			{ printf("  params -> ( list_args )%s", DELIM_SALTO_LINEA); }
    | '(' lista_argumentos argumentos ')'	{ printf("  params -> ( list_args args )%s", DELIM_SALTO_LINEA); }
;

lista_argumentos
    : argumentos ';'				{ printf("  list_args -> args ;%s", DELIM_SALTO_LINEA); }
    | lista_argumentos argumentos ';'		{ printf("  list_args -> list_args args ;%s", DELIM_SALTO_LINEA); }
;

argumentos: nombre_tipo lista_variables		{ printf("  args -> nombre_tipo list_var%s", DELIM_SALTO_LINEA); }
;

lista_variables
    : variable			{ printf("  list_var -> var%s", DELIM_SALTO_LINEA); }
    | lista_variables variable	{ printf("  list_var -> list_var var%s", DELIM_SALTO_LINEA); }
;

nombre_tipo
    : tipo				{ printf("  nom_tipo -> tipo%s", DELIM_SALTO_LINEA); }
    | tipo lista_operadores_puntero	{ printf("  nom_tipo -> tipo list_op_puntero%s", DELIM_SALTO_LINEA); }
;

variable
    : IDENTIFICADOR			{ printf("  var -> ID%s", DELIM_SALTO_LINEA); }
    | IDENTIFICADOR '=' expresion	{ printf("  var -> ID = expr%s", DELIM_SALTO_LINEA); }
;

/*****************/
/* INSTRUCCIONES */
/*****************/

instruccion
    : bloque_instrucciones
    | instruccion_expresion
    | instruccion_bifurcacion
    | instruccion_bucle
    | instruccion_salto
    | instruccion_destino_salto
    | instruccion_retorno
    | instruccion_lanzamiento_excepcion
    | instruccion_captura_excepcion
    | instruccion_vacia
;

bloque_instrucciones
    : '{' '}'    { printf("  bloque_instr -> { }%s", DELIM_SALTO_LINEA); }
    | '{' lista_declaraciones '}'    { printf("  bloque_instr -> { list_decl }%s", DELIM_SALTO_LINEA); }
    | '{' lista_instrucciones '}'    { printf("  bloque_instr -> { list_instr }%s", DELIM_SALTO_LINEA); }
    | '{' lista_declaraciones lista_instrucciones '}'    { printf("  bloque_instr -> { list_decl list_instr }%s", DELIM_SALTO_LINEA); }
;

lista_instrucciones
    : instruccion    
    | lista_instrucciones instruccion    
;
    
instruccion_expresion
    : expresion_funcional ';'    { printf("  instr_expresion -> expr_funcional%s", DELIM_SALTO_LINEA); }
    | asignacion ';'    { printf("  instr_expresion -> asig%s", DELIM_SALTO_LINEA); }
;

asignacion: expresion_indexada operador_asignacion expresion    { printf("  asig -> expr_index operador_asig expr%s", DELIM_SALTO_LINEA); }
;

operador_asignacion
    : '='
    | MULT_ASIG
    | DIV_ASIG
    | MOD_ASIG
    | SUMA_ASIG
    | RESTA_ASIG
    | DESPI_ASIG
    | DESPD_ASIG
    | AND_ASIG
    | XOR_ASIG
    | OR_ASIG
;

instruccion_bifurcacion
    : IF '(' expresion ')' instruccion    { printf("  instr_bifurcacion -> IF ( expr ) instr%s", DELIM_SALTO_LINEA); }
    | IF '(' expresion ')' instruccion ELSE instruccion    { printf("  instr_bifurcacion -> IF ( expr ) instr ELSE instr%s", DELIM_SALTO_LINEA); }
    | SWITCH '(' expresion ')' '{' lista_instrucciones_caso '}'    { printf("  instr_bifurcacion -> SWITCH ( expr ) { list_instr_caso }%s", DELIM_SALTO_LINEA); }
;

lista_instrucciones_caso
    : instruccion_caso    { printf("  list_instr_caso -> instr_caso%s", DELIM_SALTO_LINEA); }
    | lista_instrucciones_caso instruccion_caso    { printf("  list_instr_caso -> list_instr_caso instr_caso%s", DELIM_SALTO_LINEA); }
;

instruccion_caso
    : CASE expresion ':' instruccion    { printf("  instr_caso -> CASE expr : instr%s", DELIM_SALTO_LINEA); }
    | DEFAULT ':' instruccion    { printf("  instr_caso -> DEFAULT : instr%s", DELIM_SALTO_LINEA); }
;

instruccion_bucle
    : WHILE '(' expresion ')' instruccion    { printf("  instr_bucle -> WHILE ( expr ) instr%s", DELIM_SALTO_LINEA); }
    | DO instruccion WHILE '(' expresion ')' ';'    { printf("  instr_bucle -> DO instr WHILE ( expr ) ;%s", DELIM_SALTO_LINEA); }
    | FOR '(' ';' expresion ';' lista_expresiones ')' instruccion    { printf("  instr_bucle -> FOR ( ; expr ; list_expr ) instr%s", DELIM_SALTO_LINEA); }
    | FOR '(' lista_asignaciones ';' expresion ';' lista_expresiones ')' instruccion    { printf("  instr_bucle -> FOR ( list_asig ; expr list_expr ) instr%s", DELIM_SALTO_LINEA); }
;

lista_asignaciones
    : asignacion    { printf("  list_asig -> asig%s", DELIM_SALTO_LINEA); }
    | lista_asignaciones asignacion    { printf("  list_asig -> list_asig asig%s", DELIM_SALTO_LINEA); }
;

instruccion_salto
    : GOTO IDENTIFICADOR ';'    { printf("  instr_salto -> GOTO ID ;%s", DELIM_SALTO_LINEA); }
    | CONTINUE ';'    { printf("  instr_salto -> CONTINUE ;%s", DELIM_SALTO_LINEA); }
    | BREAK ';'    { printf("  instr_salto -> BREAK ;%s", DELIM_SALTO_LINEA); }
;

instruccion_destino_salto
    : IDENTIFICADOR ':' instruccion ';'    { printf("  instr_dest_salto -> ID : instr ;%s", DELIM_SALTO_LINEA); }
;

instruccion_retorno
    : RETURN ';'    { printf("  instr_ret -> RETURN ;%s", DELIM_SALTO_LINEA); }
    | RETURN expresion ';'    { printf("  instr_ret -> RETURN expr ;%s", DELIM_SALTO_LINEA); }
;

instruccion_lanzamiento_excepcion
    : THROW expresion ';'    { printf("  instr_lanz_exc -> THROW expr ;%s", DELIM_SALTO_LINEA); }
;

instruccion_captura_excepcion
    : TRY bloque_instrucciones clausulas_catch    { printf("  instr_capt_exc -> TRY bloque_instr cl_catch%s", DELIM_SALTO_LINEA); }
    | TRY bloque_instrucciones clausula_finally    { printf("  instr_capt_exc -> TRY bloque_instr cl_finally%s", DELIM_SALTO_LINEA); }
    | TRY bloque_instrucciones clausulas_catch clausula_finally    { printf("  instr_capt_exc -> TRY bloque_instr cl_catch cl_finally%s", DELIM_SALTO_LINEA); }
;

clausulas_catch
    : lista_clausula_catch_especifica    { printf("  cl_catch -> list_cl_catch_esp%s", DELIM_SALTO_LINEA); }
    | clausula_catch_general    { printf("  cl_catch -> cl_catch_gen%s", DELIM_SALTO_LINEA); }
    | lista_clausula_catch_especifica clausula_catch_general    { printf("  cl_catch -> list_cl_catch_esp cl_catch_gen%s", DELIM_SALTO_LINEA); }
;

lista_clausula_catch_especifica    
    : clausula_catch_especifica    { printf("  list_cl_catch_esp -> cl_catch_esp ;%s", DELIM_SALTO_LINEA); }
    | lista_clausula_catch_especifica clausula_catch_especifica    { printf("  list_cl_catch_esp -> list_cl_catch_esp cl_catch_esp%s", DELIM_SALTO_LINEA); }
;

clausula_catch_especifica: CATCH '(' nombre_tipo ')' bloque_instrucciones    { printf("  cl_catch_esp -> CATCH ( nom_tipo ) bloque_instr%s", DELIM_SALTO_LINEA); }
;

clausula_catch_general: CATCH bloque_instrucciones    { printf("  cl_catch_gen -> CATCH bloque_instr%s", DELIM_SALTO_LINEA); }
;

clausula_finally: FINALLY bloque_instrucciones    { printf("  cl_finally -> FINALLY bloque_instr%s", DELIM_SALTO_LINEA); }
;

instruccion_vacia: ';'    { printf("  instr_vacia -> ;%s", DELIM_SALTO_LINEA); }
;

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
    : expresion_multiplicacion '*' expresion_cast	{ printf("  expr_mult -> expr_mult * expr_cast%s", DELIM_SALTO_LINEA); }
    | expresion_multiplicacion '/' expresion_cast	{ printf("  expr_mult -> expr_mult / expr_cast%s", DELIM_SALTO_LINEA); }
    | expresion_multiplicacion '%' expresion_cast	{ printf("  expr_mult -> expr_mult %% expr_cast%s", DELIM_SALTO_LINEA); }
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

#ifdef SALIDA_VERBOSA
	yydebug = 1;
#endif

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
