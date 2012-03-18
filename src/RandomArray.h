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
