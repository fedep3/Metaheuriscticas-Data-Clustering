/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase concreta para lectura de CSVs.
 */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include "Reader.h"

using namespace std;

#ifndef _CSV_
#define _CSV_
class Csv: public Reader{
    public:
        /**
         * Constructor de la clase Reader.
         */
        Csv();
        
        /**
         * Destructor de la clase Reader.
         */
        ~Csv();

        /**
         * Lee el archivo y arma las estructuras de datos
         * necesarias.
         *
         * @param input Archivo de entrada.
         */
        virtual void read(char* input);

        /**
         * Escribe los resultados en el archivo dado.
         *
         * @param output Archivo de salida.
         * @param sol    Solución final.
         * @param k      Cantidad de clusters.
         */
        virtual void write(char* output, int* sol, int k);
};
#endif
