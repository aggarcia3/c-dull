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
	void mostrarTokenEncontrado(char*);
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
auto									{ mostrarTokenEncontrado("palabra reservada"); return AUTO; }
base									{ mostrarTokenEncontrado("palabra reservada"); return BASE; }
boolean									{ mostrarTokenEncontrado("palabra reservada"); return BOOLEAN; }
break									{ mostrarTokenEncontrado("palabra reservada"); return BREAK; }
case									{ mostrarTokenEncontrado("palabra reservada"); return CASE; }
catch									{ mostrarTokenEncontrado("palabra reservada"); return CATCH; }
class									{ mostrarTokenEncontrado("palabra reservada"); return CLASS; }
char									{ mostrarTokenEncontrado("palabra reservada"); return CHAR; }
continue								{ mostrarTokenEncontrado("palabra reservada"); return CONTINUE; }
default									{ mostrarTokenEncontrado("palabra reservada"); return DEFAULT; }
do									{ mostrarTokenEncontrado("palabra reservada"); return DO; }
double									{ mostrarTokenEncontrado("palabra reservada"); return DOUBLE; }
else									{ mostrarTokenEncontrado("palabra reservada"); return ELSE; }
enum									{ mostrarTokenEncontrado("palabra reservada"); return ENUM; }
extern									{ mostrarTokenEncontrado("palabra reservada"); return EXTERN; }
false									{ mostrarTokenEncontrado("palabra reservada"); return BOOLEANO; }
finally									{ mostrarTokenEncontrado("palabra reservada"); return FINALLY; }
float									{ mostrarTokenEncontrado("palabra reservada"); return FLOAT; }
for									{ mostrarTokenEncontrado("palabra reservada"); return FOR; }
goto									{ mostrarTokenEncontrado("palabra reservada"); return GOTO; }
if									{ mostrarTokenEncontrado("palabra reservada"); return IF; }
include									{ mostrarTokenEncontrado("palabra reservada"); return INCLUDE; }
int									{ mostrarTokenEncontrado("palabra reservada"); return INT; }
interface								{ mostrarTokenEncontrado("palabra reservada"); return INTERFACE; }
long									{ mostrarTokenEncontrado("palabra reservada"); return LONG; }
namespace								{ mostrarTokenEncontrado("palabra reservada"); return NAMESPACE; }
new									{ mostrarTokenEncontrado("palabra reservada"); return NEW; }
override								{ mostrarTokenEncontrado("palabra reservada"); return OVERRIDE; }
private									{ mostrarTokenEncontrado("palabra reservada"); return PRIVATE; }
protected								{ mostrarTokenEncontrado("palabra reservada"); return PROTECTED; }
public									{ mostrarTokenEncontrado("palabra reservada"); return PUBLIC; }
return									{ mostrarTokenEncontrado("palabra reservada"); return RETURN; }
sealed									{ mostrarTokenEncontrado("palabra reservada"); return SEALED; }
short									{ mostrarTokenEncontrado("palabra reservada"); return SHORT; }
signed									{ mostrarTokenEncontrado("palabra reservada"); return SIGNED; }
sizeof									{ mostrarTokenEncontrado("palabra reservada"); return SIZEOF; }
static									{ mostrarTokenEncontrado("palabra reservada"); return STATIC; }
struct									{ mostrarTokenEncontrado("palabra reservada"); return STRUCT; }
switch									{ mostrarTokenEncontrado("palabra reservada"); return SWITCH; }
this									{ mostrarTokenEncontrado("palabra reservada"); return THIS; }
throw									{ mostrarTokenEncontrado("palabra reservada"); return THROW; }
true									{ mostrarTokenEncontrado("palabra reservada"); return BOOLEANO; }
typedef									{ mostrarTokenEncontrado("palabra reservada"); return TYPEDEF; }
union									{ mostrarTokenEncontrado("palabra reservada"); return UNION; }
unsigned								{ mostrarTokenEncontrado("palabra reservada"); return UNSIGNED; }
using									{ mostrarTokenEncontrado("palabra reservada"); return USING; }
virtual									{ mostrarTokenEncontrado("palabra reservada"); return VIRTUAL; }
void									{ mostrarTokenEncontrado("palabra reservada"); return VOID; }
while									{ mostrarTokenEncontrado("palabra reservada"); return WHILE; }
 /* Fin de palabras clave */
{IDENTIFICADOR}								{ mostrarTokenEncontrado("identificador"); return IDENTIFICADOR; }
{DELIMITADOR}								{ mostrarTokenEncontrado("delimitador"); /* Todos los delimitadores consisten en un caracter */ return *yytext; }
\+=									{ mostrarTokenEncontrado("operador"); return SUMA_ASIG; }
-=									{ mostrarTokenEncontrado("operador"); return RESTA_ASIG; }
\*=									{ mostrarTokenEncontrado("operador"); return MULT_ASIG; }
\/=									{ mostrarTokenEncontrado("operador"); return DIV_ASIG; }
%=									{ mostrarTokenEncontrado("operador"); return MOD_ASIG; }
\<\<=									{ mostrarTokenEncontrado("operador"); return DESPI_ASIG; }
\>\>=									{ mostrarTokenEncontrado("operador"); return DESPD_ASIG; }
&=									{ mostrarTokenEncontrado("operador"); return AND_ASIG; }
\|=									{ mostrarTokenEncontrado("operador"); return OR_ASIG; }
^=									{ mostrarTokenEncontrado("operador"); return XOR_ASIG; }
\+\+									{ mostrarTokenEncontrado("operador"); return INC; }
--									{ mostrarTokenEncontrado("operador"); return DEC; }
\<\<									{ mostrarTokenEncontrado("operador"); return DESPI; }
\>\>									{ mostrarTokenEncontrado("operador"); return DESPD; }
\>=									{ mostrarTokenEncontrado("operador"); return GE; }
\<=									{ mostrarTokenEncontrado("operador"); return LE; }
==									{ mostrarTokenEncontrado("operador"); return EQ; }
!=									{ mostrarTokenEncontrado("operador"); return NEQ; }
&&									{ mostrarTokenEncontrado("operador"); return AND; }
\|\|									{ mostrarTokenEncontrado("operador"); return OR; }
-\>									{ mostrarTokenEncontrado("operador"); return PTR_ACCESO; }
{OPERADOR_UN_CARACTER}							{ mostrarTokenEncontrado("operador"); return *yytext; }
{NUMERO_DECIMAL}{EXPONENTE}						{ mostrarTokenEncontrado("constante real decimal en notación científica de mantisa entera"); return REAL; }
({NUMERO_DECIMAL})?\.{NUMERO_DECIMAL}{EXPONENTE}			{ mostrarTokenEncontrado("constante real decimal en notación científica de mantisa fraccionaria"); return REAL; }
{NUMERO_HEXADECIMAL}{EXPONENTE}						{ mostrarTokenEncontrado("constante real hexadecimal en notación científica de mantisa entera"); return REAL; }
({NUMERO_HEXADECIMAL})?\.{NUMERO_HEXADECIMAL_NO_PREF}{EXPONENTE}	{ mostrarTokenEncontrado("constante real hexadecimal en notación científica de mantisa fraccionaria"); return REAL; }
({NUMERO_DECIMAL})?\.{NUMERO_DECIMAL}					{ mostrarTokenEncontrado("constante real decimal"); return REAL; }
({NUMERO_HEXADECIMAL})?\.{NUMERO_HEXADECIMAL_NO_PREF}			{ mostrarTokenEncontrado("constante real hexadecimal"); return REAL; }
{NUMERO_OCTAL}								{ mostrarTokenEncontrado("constante entera octal"); return ENTERO; }
{NUMERO_DECIMAL}							{ mostrarTokenEncontrado("constante entera decimal"); return ENTERO; }
{NUMERO_HEXADECIMAL}							{ mostrarTokenEncontrado("constante entera hexadecimal"); return ENTERO; }
{CONSTANTE_CARACTER}							{ mostrarTokenEncontrado("caracter constante"); return CARACTER; }
{CONSTANTE_CADENA}							{
	size_t tamEntrada = yyleng;
	char* actual = yytext;
	size_t caracteresRestantes = tamEntrada;

	mostrarTokenEncontrado("cadena constante");

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

void mostrarTokenEncontrado(char* tipo) {
	printf("Línea %u, %s: %s%s", numLinea, tipo, yytext, DELIM_SALTO_LINEA);
}

void mostrarErrorSintaxis(void) {
	fprintf(stderr, "Línea %u, token no reconocido: %s - se aborta el análisis%s", numLinea, yytext, DELIM_SALTO_LINEA);
	codigoSalida = EXIT_FAILURE;
}

int yywrap() {
	return 1;
}
