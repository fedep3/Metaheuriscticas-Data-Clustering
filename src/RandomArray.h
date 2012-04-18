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
#include <ctime>
#include <fcntl.h>
#include "mt.h"

#ifndef _RANDOM_ARRAY_
#define _RANDOM_ARRAY_

using namespace std;

class RandomArray{
    public:
        /**
         * @param size Size of the array.
         */
        RandomArray(int size);

        /**
         * Resets the array.
         */
        void reset();

        /**
         * @return Random value of the array. This value will never be repeated, unless
         *         the array is reset.
         */
        int get();

        /**
         * Destructor.
         */
        ~RandomArray();

        /**
         * Initial length of the array.
         */
        int initialLength;

        /**
         * Current size of the array.
         */
        int length;

        /**
         * Last element in the array.
         */
        int last;

        /**
         * Array.
         */
        int* rarr;

        /**
          * Random number generator.
          */
        MTStore drand;
};
#endif
