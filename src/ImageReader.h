/*
Copyright (c) 2011 Alexander De Sousa, Federico Ponte

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
Software), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase abstracta en donde se definen las funciones básicas de
 * lectura de imágenes.
 */
#include <cstdio>
#include <cstdlib>
#include <vector>
#include "Reader.h"
#include "lodepng.h"

using namespace std;

#ifndef _IMAGE_READER_
#define _IMAGE_READER_
class ImageReader: public Reader{
    public:
        /**
         * Constructor de la clase ImageReader.
         */
        ImageReader(){};
        
        /**
         * Destructor de la clase ImageReader.
         */
        ~ImageReader(){};

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
        virtual void write(char* output, int* sol, int k);

    protected:
        /**
         * Genera colores dependiendo de la cantidad de clusters
         * dado.
         *
         * @param colors Arreglo de colores.
         * @param k      Cantidad de clusters.
         */
        void generateColors(float **colors, int k);

        /**
         * Ancho de la imagen.
         */
        int width;

        /**
         * Alto de la imagen.
         */
        int height;
};
#endif
