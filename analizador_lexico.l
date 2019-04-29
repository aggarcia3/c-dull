 /* Definiciones */
%{
	/* Importar definiciones de elementos terminales reconocidos por Bison */
	#include "analizador_sintactico.tab.h"
        #include <stdlib.h>
        #include <stdio.h>

	/* Los caracteres delimitadores de saltos de línea que se usarán para la salida de datos. En formato Unix, los saltos de línea se indican por el caracter Line Feed (\n) */
	#define DELIM_SALTO_LINEA "\n"

	/* El número de línea en el que se encuentra actualmente procesando la entrada el autómata */
	unsigned int numLinea = 1;
	/* El código de salida del programa a devolver al SO */
	extern int codigoSalida;

	void mostrarErrorSintaxis(void);
%}

PALABRA_RESERVADA		auto|base|boolean|break|case|catch|class|char|continue|default|do|double|else|enum|extern|false|finally|float|for|goto|if|include|int|interface|long|namespace|new|override|private|protected|public|return|sealed|short|signed|sizeof|static|struct|switch|this|throw|true|typedef|union|unsigned|using|virtual|void|while
IDENTIFICADOR			[a-zA-Z_][a-zA-Z_0-9]*
NUMERO_DECIMAL			[0-9]+
NUMERO_HEXADECIMAL		0[xX][0-9a-fA-F]+
NUMERO_HEXADECIMAL_NO_PREF	[0-9a-fA-F]+
NUMERO_OCTAL			0[0-7]+
EXPONENTE			[eE][+-]?[0-9]+
 /* Nota: las siguientes dos expresiones regulares asumen que SALTO_LINEA es \n. De ser cambiado el valor de tal alias, deben de cambiarse estos también, si es necesario */
CONSTANTE_CARACTER		'[^\n'\\]'|'(\\\'|\\\"|\\\?|\\\\|\\n|\\r|\\t|\\v)'|'\\(37[0-7]|3[0-6][0-7]|[0-2][0-7]{2}|[0-7]{1,2})'|'\\x[0-9a-fA-F]{1,2}'
CONSTANTE_CADENA		\"([^\n"\\]|\\\'|\\\"|\\\?|\\\\|\\n|\\r|\\t|\\v|\\(37[0-7]|3[0-6][0-7]|[0-2][0-7]{2}|[0-7]{1,2})|\\x[0-9a-fA-F]{1,2}|\\\n)*\"
DELIMITADOR			\(|\)|\{|\}|\?|:|;|,
OPERADOR_UN_CARACTER		\+|-|\*|\/|%|\.|&|\[|\]|~|\||^|<|>|!|=
COMENTARIO_LINEA		\/\/
COMENTARIO_MULTI		\/\*
FIN_COMENTARIO_MULTI		\*\/

 /* Los caracteres delimitadores de saltos de línea que se considerarán en el fichero de código fuente */
SALTO_LINEA			\n

 /* Condiciones de arranque */
%x COMENTARIO_LINEA
%x COMENTARIO_MULTI

%%
 /* Reglas */
 /* Palabras clave */
auto									{ return AUTO; }
base									{ return BASE; }
boolean									{ return BOOLEAN; }
break									{ return BREAK; }
case									{ return CASE; }
catch									{ return CATCH; }
class									{ return CLASS; }
char									{ return CHAR; }
continue								{ return CONTINUE; }
default									{ return DEFAULT; }
do									{ return DO; }
double									{ return DOUBLE; }
else									{ return ELSE; }
enum									{ return ENUM; }
extern									{ return EXTERN; }
false									{ return BOOLEANO; }
finally									{ return FINALLY; }
float									{ return FLOAT; }
for									{ return FOR; }
goto									{ return GOTO; }
if									{ return IF; }
include									{ return INCLUDE; }
int									{ return INT; }
interface								{ return INTERFACE; }
long									{ return LONG; }
namespace								{ return NAMESPACE; }
new									{ return NEW; }
override								{ return OVERRIDE; }
private									{ return PRIVATE; }
protected								{ return PROTECTED; }
public									{ return PUBLIC; }
return									{ return RETURN; }
sealed									{ return SEALED; }
short									{ return SHORT; }
signed									{ return SIGNED; }
sizeof									{ return SIZEOF; }
static									{ return STATIC; }
struct									{ return STRUCT; }
switch									{ return SWITCH; }
this									{ return THIS; }
throw									{ return THROW; }
true									{ return BOOLEANO; }
typedef									{ return TYPEDEF; }
union									{ return UNION; }
unsigned								{ return UNSIGNED; }
using									{ return USING; }
virtual									{ return VIRTUAL; }
void									{ return VOID; }
while									{ return WHILE; }
 /* Fin de palabras clave */
{IDENTIFICADOR}								{ return IDENTIFICADOR; }
{DELIMITADOR}								{ /* Todos los delimitadores consisten en un caracter */ return *yytext; }
\+=									{ return SUMA_ASIG; }
-=									{ return RESTA_ASIG; }
\*=									{ return MULT_ASIG; }
\/=									{ return DIV_ASIG; }
%=									{ return MOD_ASIG; }
\<\<=									{ return DESPI_ASIG; }
\>\>=									{ return DESPD_ASIG; }
&=									{ return AND_ASIG; }
\|=									{ return OR_ASIG; }
^=									{ return XOR_ASIG; }
\+\+									{ return INC; }
--									{ return DEC; }
\<\<									{ return DESPI; }
\>\>									{ return DESPD; }
\>=									{ return GE; }
\<=									{ return LE; }
==									{ return EQ; }
!=									{ return NEQ; }
&&									{ return AND; }
\|\|									{ return OR; }
-\>									{ return PTR_ACCESO; }
{OPERADOR_UN_CARACTER}							{ return *yytext; }
{NUMERO_DECIMAL}{EXPONENTE}						{ return REAL; }
({NUMERO_DECIMAL})?\.{NUMERO_DECIMAL}{EXPONENTE}			{ return REAL; }
{NUMERO_HEXADECIMAL}{EXPONENTE}						{ return REAL; }
({NUMERO_HEXADECIMAL})?\.{NUMERO_HEXADECIMAL_NO_PREF}{EXPONENTE}	{ return REAL; }
({NUMERO_DECIMAL})?\.{NUMERO_DECIMAL}					{ return REAL; }
({NUMERO_HEXADECIMAL})?\.{NUMERO_HEXADECIMAL_NO_PREF}			{ return REAL; }
{NUMERO_OCTAL}								{ return ENTERO; }
{NUMERO_DECIMAL}							{ return ENTERO; }
{NUMERO_HEXADECIMAL}							{ return ENTERO; }
{CONSTANTE_CARACTER}							{ return CARACTER; }
{CONSTANTE_CADENA}							{
	size_t tamEntrada = yyleng;
	char* actual = yytext;
	size_t caracteresRestantes = tamEntrada;

	/* Mientras queden al menos dos caracteres en yytext, ir contando los saltos de línea escapados */
	while (caracteresRestantes > 1) {
		/* ¿Estamos en un salto de línea escapado en la posición actual, o en la siguiente a la actual? */
		if (*actual == '\\' && *(actual + 1) == '\n' || caracteresRestantes > 2 && *(actual + 1) == '\\' && *(actual + 2) == '\n') {
			++numLinea;
		}
		/* Avanzar el puntero de la cadena dos caracteres hacia delante */
		actual += 2;
		caracteresRestantes -= 2;
	}

	return CADENA;
}
{COMENTARIO_LINEA}							BEGIN(COMENTARIO_LINEA);
{COMENTARIO_MULTI}							BEGIN(COMENTARIO_MULTI);
<COMENTARIO_LINEA>{SALTO_LINEA}						{ BEGIN(INITIAL); ++numLinea; }
<COMENTARIO_MULTI>{FIN_COMENTARIO_MULTI}				BEGIN(INITIAL);
<COMENTARIO_MULTI,INITIAL>{SALTO_LINEA}					++numLinea;
 /* Esta regla, al coincidir con un solo caracter y estar al final, tendrá prioridad mínima para Flex */
<INITIAL>[^ \t\n]							{ mostrarErrorSintaxis(); yyterminate(); }
 /* Avisar si encontramos comentarios multilínea no finalizados */
<COMENTARIO_MULTI><<EOF>>						{ puts("Aviso: comentario multilínea no finalizado en código fuente"); yyterminate(); }
 /* Descartar caracteres que no encajen con otras reglas, en caso de que hubiere */
<INITIAL,COMENTARIO_LINEA,COMENTARIO_MULTI>.				;

%%
 /* Código del usuario */

void mostrarErrorSintaxis(void) {
	fprintf(stderr, "Línea %u, token no reconocido: %s - se aborta el análisis%s", numLinea, yytext, DELIM_SALTO_LINEA);
	codigoSalida = EXIT_FAILURE;
}

int yywrap() {
	return 1;
}