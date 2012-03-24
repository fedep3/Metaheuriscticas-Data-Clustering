/*
Data Clustering Metaheuristics focused on images.
Copyright (C) 2011 Alexander De Sousa(prof.etadel2@gmail.com), 
                                                Federico Ponte(fedep3@gmail.com)

This program is free software; you can redistribute it and/or modify it under 
the terms of the GNU General Public License as published by the Free Software 
Foundation; either version 2 of the License, or (at your option) any later 
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY 
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
PARTICULAR PURPOSE. See the GNU General Public License for more details.
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
