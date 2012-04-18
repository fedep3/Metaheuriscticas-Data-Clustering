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
#include "ImageReader.h"

using namespace Magick;
using namespace std;

/**
 * Generates RGB colors depending on the quantity of clusters.
 *
 * @param K Quantity of clusters.
 *
 * @return Generated colors.
 */
float** ImageReader::generateColors(int K) {
    int count = 0, base = 2;
    int i, j, k;

    float** colors = new float*[K];

    
    for(i = 0; i < K; ++i)
        colors[i] = new float[3];

    while(true){
        i = base * base * base;
        if(K <= i) break;
        ++base;
    }

    for(i = 0; i < base && count < K; ++i){
        for(j = 0; j < base && count < K; ++j){
            for(k = 0; k < base && count < K; ++k){
                colors[count][0] = 255.0 * ( ( (float) i )/ ( (float) base) );
                colors[count][1] = 255.0 * ( ( (float) j )/ ( (float) base) );
                colors[count][2] = 255.0 * ( ( (float) k )/ ( (float) base) );
                ++count;
            }
        }
    }

    return colors;
}

/**
 * Reads the file.
 *
 * @param inputFile Input file.
 */
void ImageReader::read(const char* inputFile) {
    int i, j, k;
    try {
        Image img(inputFile);

        width  = img.columns();
        height = img.rows();
        data.N = width * height;
        
        if(img.type() == GrayscaleType || img.type() == GrayscaleMatteType)
            data.M = 1;
        else
            data.M = 3;

        data.pattern = new float*[data.N];
        for(i = 0; i < data.N; ++i)
            data.pattern[i] = new float[data.M];

        k = 0;
        for(i = 0; i < width; ++i) {
            for(j = 0; j < height; ++j) {
                if(data.M == 1) {
                    ColorGray gray(img.pixelColor(i, j));
                    data.pattern[k][0] = 255.0 * gray.shade();
                } else {
                    ColorRGB rgb(img.pixelColor(i, j));
                    data.pattern[k][0] = 255.0 * rgb.red();
                    data.pattern[k][1] = 255.0 * rgb.green();
                    data.pattern[k][2] = 255.0 * rgb.blue();
                }
                ++k;
            }
        }
    } catch ( Magick::Exception & error) {
        printf("Magick++ exception: %s\n", error.what());
        exit(1);
    }
}

/**
 * Writes the results in an output file.
 *
 * @param outputFile Output file.
 * @param solution   Solution to be written.
 * @param centroid   Centroids for the solution.
 * @param K          Quantity of clusters for the solution.
 */
void ImageReader::write(const char* outputFile, int* solution, float** centroid, int K) {
    int i, j, k, R, G, B;
    float red, green, blue;
    float** colors;

    if(generate)
        colors = generateColors(K);
    else
        colors = centroid;

    if(!generate && data.M == 1)
        R = G = B = 0;
    else {
        R = 0;
        G = 1;
        B = 2;
    }

    try {
        Image img( Geometry(width,height), "black");

        k = 0;
        for(i = 0; i < width; ++i) {
            for(j = 0; j < height; ++j) {
                red   = colors[solution[k]][R] / 255.0;
                green = colors[solution[k]][G] / 255.0;
                blue  = colors[solution[k]][B] / 255.0;
                ColorRGB rgb(red, green, blue);
                img.pixelColor(i, j, rgb);
                ++k;
            }
        }

        img.write(outputFile);
    } catch (Magick::Exception & error) {
        printf("Caught Magick++ exception: %s\n", error.what());
        exit(1);
    }

    if(generate) {
        for(i = 0; i < K; ++i)
            delete [] colors[i];
        delete [] colors;
    }
}
