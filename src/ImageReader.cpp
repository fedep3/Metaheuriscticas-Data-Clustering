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
 * Clase abstracta en donde se definen las funciones básicas de
 * lectura de imágenes.
 */

#include "ImageReader.h"

/**
 * Genera colores dependiendo de la cantidad de clusters dado.
 *
 * @param colors Arreglo de colores.
 * @param k      Cantidad de clusters.
 */
void ImageReader::generateColors(float **colors, int k){
    int count = 0;

    //M = 3
    int base = 2;
    int i, j, l;

    while(true){
        i = base * base * base;
        if(k <= i) break;
        ++base;
    }

    for(i = 0; i < base && count < k; ++i){
        for(j = 0; j < base && count < k; ++j){
            for(l = 0; l < base && count < k; ++l){
                colors[count][0] = 255.0 * ( ( (float) i )/ ( (float) base) );
                colors[count][1] = 255.0 * ( ( (float) j )/ ( (float) base) );
                colors[count][2] = 255.0 * ( ( (float) l )/ ( (float) base) );
                ++count;
            }
        }
    }
}

/**
 * Escribe los resultados en el archivo dado. Genera una imagen
 * PNG.
 *
 * @param output Archivo de salida.
 * @param sol    Solución final.
 * @param k      Cantidad de clusters.
 */
void ImageReader::write(char* output, int* sol, int k){
    int i;
    float **colors;
    vector<unsigned char> image;
    image.resize(N * 4);
    vector<unsigned char>::iterator imageIterator = image.begin();

    if( (colors = (float **) calloc(k, sizeof(float *) )) == NULL){
        fprintf(stderr, "Error pidiendo memoria.\n");
        exit(1);
    }

    for(i = 0; i < k; ++i)
        colors[i] = new float[M];

    generateColors(colors, k);

    i = 0;
    while (imageIterator != image.end()) {
        *imageIterator = (unsigned char) (colors[sol[i]][0]);
        imageIterator++;
        *imageIterator = (unsigned char) (colors[sol[i]][1]);
        imageIterator++;
        *imageIterator = (unsigned char) (colors[sol[i]][2]);
        imageIterator++;
        *imageIterator = (unsigned char) 255;
        imageIterator++;
        ++i;
    }
    LodePNG::encode(output, image, (int)width, (int)height);

    //Hacer Tabla de resultados. Probablemente hay que agregar
    //parámetros.

    for(i = 0; i < k; ++i)
        delete [] colors[i];
    free(colors);
}
