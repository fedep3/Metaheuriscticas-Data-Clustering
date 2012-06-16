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
 * Abstract class to read the data set for a data clustering algorithm
 * and write its results in a output file.
 */
#include <cstdio>
#include <cstdlib>
#include "Data.h"

using namespace std;

#ifndef _READER_
#define _READER_
class Reader{
    public:
        /**
         * Constructor.
         */
        Reader();
        
        /**
         * Destructor.
         */
        virtual ~Reader();

        /**
         * Reads the file.
         *
         * @param inputFile Input file.
         */
        virtual void read(const char* inputFile) = 0;

        /**
         * Writes the results in an output file.
         *
         * @param outputFile Output file.
         * @param solution   Solution to be written.
         * @param centroid   Centroids for the solution.
         * @param K          Quantity of clusters for the solution.
         */
        virtual void write(const char* outputFile, int* solution, float** centroid, int K) = 0;

        /**
         * Data set information.
         */
        Data data;
};
#endif
