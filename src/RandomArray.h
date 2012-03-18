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
#include <iostream>
#include <fcntl.h>
#include "mtrand.h"

#ifndef _RANDOM_ARRAY_
#define _RANDOM_ARRAY_

using namespace std;

class RandomArray{
    public:

        /**
         * Constructor del arreglo con aleatoriedad.
         *
         * @param size Tamaño del arreglo.
         */
        RandomArray(int size);

        /**
         * Para reiniciar la clase
         *
         * @param size Tamaño del arreglo.
         */
        void reset(int size);

        /**
         * Obtiene un valor aleatorio y sin repetición del arreglo.
         *
         * @return Valor del arreglo.
         */
        int get();

        /**
         * Destructor de un RandomArray.
         */
        ~RandomArray();

        /**
         * Tamaño virtual del arreglo.
         */
        int length;

        /**
         * Último elemento virtual del arreglo.
         */
        int last;

        /**
         * Arreglo.
         */
        int* rarr;

        MTRand drand;
};
#endif
