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
 * Clase concreta donde se definen las funciones y variables
 * necesarias para ejecutar la metaheurística K-means.
 */

#include "Metaheuristic.h"
#include "RandomArray.h"

#ifndef _KMEANS_
#define _KMEANS_

using namespace std;

class Kmeans : public Metaheuristic{
    public:

        /**
         * Constructor de la clase Kmeans.
         *
         * @param d   Datos del problema.
         * @param m   Dimensión de cada dato.
         * @param n   Cantidad de datos.
         * @param k   Cantidad de clusters (iniciales).
         * @param met Métrica.
         * @param reps Cantidad de repeticiones sin mejora.
         */
        Kmeans(float** _d, int _m, int _n, int _k, int _met, int _reps);

        /**
         * Toma un vector de centroides e inicializa el arreglo
         * de centroides del K-means.
         *
         * @param cent Centroides. 
         */
        void setCentroids(int* sol, float** cent, int type);

        /**
         * Inicializa los centroides de los clusters. No se
         * utiliza si ya se tienen centroides.
         */
        virtual void initialize();

        /**
         * Ejecuta la metaheurística con los datos dados.
         *
         * @param type Si se utilizará una función objetivo de
         *             maximización o de minimización.
         */
        virtual void run(int type);

        /**
         * Destructor de la clase Kmeans.
         */
        ~Kmeans();
    protected:
        /**
         * Cantidad de repeticiones sin mejora.
         */
        int REPS;

        /**
         * Si fue inicializado con un arreglo de centroides.
         */
        bool initialized;
};
#endif
