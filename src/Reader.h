/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase abstracta en donde se definen las funciones básicas de
 * lectura.
 */
#include <cstdio>
#include <cstdlib>

using namespace std;

#ifndef _READER_
#define _READER_
class Reader{
    public:
        /**
         * Constructor de la clase Reader.
         */
        Reader();
        
        /**
         * Destructor de la clase Reader.
         */
        ~Reader();

        /**
         * Lee el archivo y arma las estructuras de datos
         * necesarias.
         *
         * @param input Archivo de entrada.
         */
        virtual void read(char* input) = 0;

        /**
         * Escribe los resultados en el archivo dado.
         *
         * @param output Archivo de salida.
         * @param sol    Solución final.
         * @param k      Cantidad de clusters.
         */
        virtual void write(char* output, int* sol, int k) = 0;

        /**
         * Dimensiones de cada objeto.
         */
        int M;

        /**
         * Cantidad de objetos.
         */
        int N;

        /**
         * Objetos.
         */
        double **data;
};
#endif
