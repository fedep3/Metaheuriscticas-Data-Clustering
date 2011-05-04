/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase que representa una hormiga usada valga la redundancia en el algoritmo
 * de Hormiga.
 */

#ifndef _Ant_
#define _Ant_

#include <cstdio>
#include <time.h>
#include <vector>
#include "Metaheuristic.h"

using namespace std;

#define MM 3z


class Ant{
    public:

        /*
         * Constructor de la clase Ant.
         */
        Ant();

        /*
         * Destructor de la clase Ant.
         */
        ~Ant();

        /*
         * Cambia el estado de la hormiga ya que tomó un pixel.
         * @param p Pixel que agarró.
         */
        void pick(int p);

        /*
         * Suelta el pixel que tiene y modifica su memoria agregando a ella
         * donde lo dejó.
         * @param cell Célula donde va a dejar el pixel.
         */
        void drop(int cell);

        /*
         * Pixel que carga.
         */
        int pixel;

        /*
         * Booleano que indica si carga o no un pixel.
         */
        bool free;

        /*
         * Arreglo de memoria.
         */
        int *memory;

        /*
         * Tamaño actual de la memoria..
         */
        int msize;


};
#endif
