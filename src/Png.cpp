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
 * Clase concreta en donde se definen las funciones básicas de
 * lectura de CSV.
 */
#include "Png.h"

/**
 * Constructor de la clase Csv.
 */
Png::Png()
: ImageReader(){}

/**
 * Destructor de la clase Csv.
 */
Png::~Png(){
    int i;
    for(i = 0; i < N; ++i)
        delete [] data[i];
    free(data);
}

/**
 * Lee el archivo y arma las estructuras de datos
 * necesarias.
 *
 * @param input Archivo de entrada.
 */
void Png::read(char* input){
    int i, j;
    vector<unsigned char> buffer, image;

    LodePNG::loadFile(buffer, input);
    LodePNG::Decoder decoder; 
    decoder.inspect(buffer);
    if(!decoder.hasError()){
        if(decoder.isGreyscaleType()) M = 1;
        else                          M = 3;
    }else{
        printf("Error al leer la imagen.\n");
        exit(1);
    }

    switch(M){
        case 3:
            decoder.decode(image, buffer.empty() ? 0 : &buffer[0], (unsigned)buffer.size());

            if (decoder.getChannels() < 3 || decoder.getBpp() < 24) {
                fprintf(stderr,"La imagen debe ser de 8 bits por canal.\n");
                exit(1);
            }

            width  = decoder.getWidth();
            height = decoder.getHeight();
            N = width * height;
            break;
        case 1:
            decoder.decode(image, buffer.empty() ? 0 : &buffer[0], (unsigned)buffer.size());

            width  = decoder.getWidth();
            height = decoder.getHeight();
            N = width * height;
    }

    if((data = (float **) calloc(N, sizeof(float *))) == NULL){
        fprintf(stderr, "Error pidiendo memoria.\n");
        exit(1);
    }
    for(i = 0; i < N; ++i)
        data[i] = new float[M];

    vector<unsigned char>::iterator imageIterator = image.begin();

    switch(M){
        case 3:
            i = 0;

            while (imageIterator != image.end()) {
                for(j = 0; j < 3; ++j){
                    data[i][j] = (float) (*imageIterator);
                    imageIterator++;
                }
                imageIterator++; 
                ++i;
            }
            break;
        case 1:
            while (imageIterator != image.end()) {
                data[i][0] = (*imageIterator);
                imageIterator++;
                ++i;
            }
    }
}
