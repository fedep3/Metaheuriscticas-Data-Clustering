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
 * Clase para arreglos aleatorios.
 *
 */
#include <iostream>
#include <fcntl.h>
#include "mt.h"

#ifndef _RANDOM_ARRAY_
#define _RANDOM_ARRAY_

using namespace std;

class RandomArray{
    public:

        /**
         * Constructor del arreglo con aleatoriedad.
         *
         * @param size Tamaño del arreglo.
         */
        RandomArray(int size);

        /**
         * Para reiniciar la clase
         *
         * @param size Tamaño del arreglo.
         */
        void reset(int size);

        /**
         * Obtiene un valor aleatorio y sin repetición del arreglo.
         *
         * @return Valor del arreglo.
         */
        int get();

        /**
         * Destructor de un RandomArray.
         */
        ~RandomArray();

        /**
         * Tamaño virtual del arreglo.
         */
        int length;

        /**
         * Último elemento virtual del arreglo.
         */
        int last;

        /**
         * Arreglo.
         */
        int* rarr;

        /**
          * Generador de números aleatorios.
          */
        MTStore * drand;
};
#endif
