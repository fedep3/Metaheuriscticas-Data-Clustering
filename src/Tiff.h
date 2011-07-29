/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase concreta en donde se definen las funciones básicas de
 * lectura de imágenes TIFF.
 */
#include <tiffio.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include "ImageReader.h"

#ifndef _TIFF_Reader_
#define _TIFF_Reader_

using namespace std;

class Tiff: public ImageReader{
    public:

        /**
         * Constructor de la clase Tiff.
      
         */
        Tiff();

        /**
         * Destructor de la clase Tiff.
         */
        ~Tiff();

        /**
         * Lee el archivo y arma las estructuras de datos
         * necesarias.
         *
         * @param input Archivo de entrada.
         */
        virtual void read(char* input);
};
#endif
