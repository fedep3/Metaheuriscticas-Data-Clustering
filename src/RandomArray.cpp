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
    int i;

    length = size;
    last   = length - 1;

    rarr = new int[length];
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

    int r = rand() % length;

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
    delete rarr;
}
