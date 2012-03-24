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
