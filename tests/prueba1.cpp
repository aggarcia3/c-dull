namespace inutil {
	using standard.math;
	using standard.io;

	double PI 31.41592e-1 // Definicion de una constante

	int main(int argc, char *argv[]) {
		/* Variables */
		float area, radio;
		printf("\nRadio de la /*circunferencia*/\
			\151\144\x69\157\x74\141: ");
		scanf("%f", &radio); /* Entrada de dato */

		printf("\\hola22");

		// Calculo del area
		area = PI * pow(radio, 2);

		/* El resultado del �rea se saca por la "consola":
		   se trata de un n�mero real */
		printf("\nArea de la \"circunferencia\": %f", area); printf("\n");

		return 0;
	}
}
