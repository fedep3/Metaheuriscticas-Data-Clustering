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
#include "RandomArray.h"

/**
 * Constructor del arreglo con aleatoriedad.
 *
 * @param size Tamaño del arreglo.
 */
RandomArray::RandomArray(int size){

    drand = new MTStore();
    drand->mtRandomInit(drand, time(NULL), K_2M31);

    int i;

    length = size;
    last   = length - 1;

    rarr = new int[length];
    for(i = 0; i < length; ++i){
        rarr[i] = i;
    }
}

/**
 * Para reiniciar la clase
 *
 * @param size Tamaño del arreglo.
 */
void RandomArray::reset(int size){
    int i;

    length = size;
    last   = length - 1;

    for(i = 0; i < length; ++i){
        rarr[i] = i;
    }
}

/**
 * Obtiene un valor aleatorio y sin repetición del arreglo.
 *
 * @return Valor del arreglo.
 */
int RandomArray::get(){
    if(length == 0) return 0;

    int r = length*mtGetRandomFloat(drand);

    int out = rarr[r];

    rarr[r] = rarr[last];
    --last;
    --length;

    return out;
}

/**
 * Destructor de un RandomArray.
 */
RandomArray::~RandomArray(){
    delete [] rarr;
    delete drand;
}
