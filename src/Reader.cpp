/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase abstracta en donde se definen las funciones básicas de
 * lectura.
 */

#include "Reader.h"

/**
 * Constructor de la clase Reader.
 */
Reader::Reader(){ }

/**
 * Destructor de la clase Reader.
 */
Reader::~Reader(){

    for(int i = 0; i < N; i++)
        delete [] data[i];
    free(data);

}
