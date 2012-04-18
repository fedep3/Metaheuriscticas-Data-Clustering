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
 * Concrete class to read images files for a data clustering algorithm and
 * write its results in a image file.
 */
#include <cstdio>
#include <cstdlib>
#include <vector>
#include <Magick++.h>
#include "Reader.h"

using namespace std;

#ifndef _IMAGE_READER_
#define _IMAGE_READER_

class ImageReader: public Reader{
    public:
        /**
         * @param generate Whether the colors should be generated or not for the output.
         */
        ImageReader(bool generate = false) : Reader() {
            this->generate = generate;
        }
        
        /**
         * Destructor.
         */
        ~ImageReader(){}

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
        void write(const char* outputFile, int* solution, float** centroid, int K);

    private:
        /**
         * Generates RGB colors depending on the quantity of clusters.
         *
         * @param K Quantity of clusters.
         *
         * @return Generated colors.
         */
        float** generateColors(int K);

        /**
         * Whether the colors should be generated or not when the output
         * image is created.
         */
        bool generate;

    protected:
        /**
         * Width of the image.
         */
        int width;

        /**
         * Height of the image.
         */
        int height;
};
#endif
