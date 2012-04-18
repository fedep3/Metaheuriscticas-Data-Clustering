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
 * Container for the data of an instance of the clustering problem.
 */
#include <cstdio>
#include <cstdlib>
#ifndef _DATA_
#define _DATA_

using namespace std;

class Data {
    public:
        /**
         * Instantiates an empty pattern container.
         */
        Data();

        /**
         * Sets the Data information.
         *
         * @param pattern Instance patterns.
         * @param N       Quantity of patters.
         * @param M       Size of every pattern.
         */
        void setDataInfo(float** pattern, int N, int M);

        /**
         * Data destructor.
         */
        ~Data();

        /**
         * Patterns.
         */
        float** pattern;

        /**
         * Quantity of patterns.
         */
        int N;

        /**
         * Size of every patterns.
         */
        int M;
};
#endif
