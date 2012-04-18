/**
 * @copyright
 *
 * Project Athena for Data Clustering Metaheuristics focused on images.
 *
 * Copyright (C) 2011 Alexander De Sousa (alexanderjosedesousa@gmail.com),
 *                    Federico Ponte     (fedep3@gmail.com)
 *
 * This program is free software; you can redistribute it and/or modify it under 
 * the terms of the GNU General Public License as published by the Free Software 
 * Foundation; either version 2 of the License, or (at your option) any later 
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * @author Alexander De Sousa (alexanderjosedesousa@gmail.com),
 *         Federico Ponte     (fedep3@gmail.com)
 *
 * @section Description
 *
 * Creates a random array that never repeats a number.
 */
#include "RandomArray.h"

/**
 * @param size Size of the array.
 */
RandomArray::RandomArray(int size){
    drand.mtRandomInit(&drand, time(NULL), K_2M31);

    int i;

    initialLength = size;

    length = initialLength;
    last   = length - 1;

    rarr = new int[length];
    for(i = 0; i < length; ++i)
        rarr[i] = i;
}

/**
 * Resets the array.
 */
void RandomArray::reset(){
    int i;

    length = initialLength;
    last   = length - 1;

    for(i = 0; i < length; ++i)
        rarr[i] = i;
}

/**
 * @return Random value of the array. This value will never be repeated, unless
 *         the array is reset.
 */
int RandomArray::get() {
    if(length == 0) return 0;

    int r = length * mtGetRandomFloat(&drand);

    int out = rarr[r];

    rarr[r] = rarr[last];
    --last;
    --length;

    return out;
}

/**
 * Destructor.
 */
RandomArray::~RandomArray(){
    delete [] rarr;
}
