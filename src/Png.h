/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase concreta en donde se definen las funciones básicas de
 * lectura de imágenes PNG.
 */
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include "ImageReader.h"

#ifndef _PNG_Reader_
#define _PNG_Reader_

using namespace std;

class Png: public ImageReader{
    public:

        /**
         * Constructor de la clase Png.
      
         */
        Png();

        /**
         * Destructor de la clase Png.
         */
        ~Png();

        /**
         * Lee el archivo y arma las estructuras de datos
         * necesarias.
         *
         * @param input Archivo de entrada.
         */
        virtual void read(char* input);
};
#endif
