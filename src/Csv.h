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
 * Concrete class to read CSV files for a data clustering algorithm and
 * write its results in a file.
 */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include "Reader.h"

using namespace std;

#ifndef _CSV_
#define _CSV_
class Csv: public Reader{
    public:
        /**
         * Constructor.
         */
        Csv();

        /**
         * Destructor.
         */
        ~Csv();

        /**
         * Reads the file.
         *
         * @param inputFile Input file.
         */
        virtual void read(const char* inputFile);

        /**
         * Writes the results in an output file.
         *
         * @param outputFile Output file.
         * @param solution   Solution to be written.
         * @param centroid   Centroids for the solution.
         * @param K          Quantity of clusters for the solution.
         */
        virtual void write(const char* outputFile, int* solution, float** centroid, int K);
};
#endif
