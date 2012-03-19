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
 * Clase para arreglos aleatorios.
 *
 */
#include "RandomArray.h"

/**
 * Constructor del arreglo con aleatoriedad.
 *
 * @param size Tamaño del arreglo.
 */
RandomArray::RandomArray(int size){

    int randomData = open("/dev/random", O_RDONLY);
    int seed;
    read(randomData, &seed, sizeof seed);
    close(randomData);

    drand.seed(seed);

    int i;

    length = size;
    last   = length - 1;

    rarr = new int[length];
    for(i = 0; i < length; ++i){
        rarr[i] = i;
    }
}

/**
 * Para reiniciar la clase
 *
 * @param size Tamaño del arreglo.
 */
void RandomArray::reset(int size){
    int i;

    length = size;
    last   = length - 1;

    for(i = 0; i < length; ++i){
        rarr[i] = i;
    }
}

/**
 * Obtiene un valor aleatorio y sin repetición del arreglo.
 *
 * @return Valor del arreglo.
 */
int RandomArray::get(){
    if(length == 0) return 0;

    int r = length*drand();

    int out = rarr[r];

    rarr[r] = rarr[last];
    --last;
    --length;

    return out;
}

/**
 * Destructor de un RandomArray.
 */
RandomArray::~RandomArray(){
    delete [] rarr;
}
