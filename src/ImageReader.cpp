/*
Copyright (c) 2011 Alexander De Sousa, Federico Ponte

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
Software), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
