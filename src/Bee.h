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
 * necesarias para ejecutar el algoritmo de abeja.
 */
#include <cstdio>
#include "RandomArray.h"
#include "Metaheuristic.h"

#ifndef _BEE_
#define _BEE_

using namespace std;

class Bee : public Metaheuristic{
    public:

        /**
         * Constructor de la clase GA.
         *
         * @param _d    Datos del problema.
         * @param _m    Dimensión de cada dato.
         * @param _n    Cantidad de datos.
         * @param _k    Cantidad de clusters (iniciales).
         * @param _i    Cantidad de individuos.
         * @param _ms   Cantidad de sitios seleccionados.
         * @param _es   Cantidad de sitios élite.
         * @param _eb   Cantidad de abejas a sitios élite.
         * @param _ob   Cantidad de abejas a sitios no-élite.
         * @param _met  Métrica.
         * @param _reps Repeticiones sin mejora.
         */
        Bee(float** _d, int _m, int _n, int _k, int _i,
           int _ms, int _es, int _eb, int _ob,
           int _met, int _reps);

        /**
         * Inicializa la población. (Soluciones, centroides y
         * tamanos de clusters.
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
         * Destructor de la clase Bee.
         */
        ~Bee();

    protected:
        /**
         * Selecciona los sitios que exploraran las abejas.
         *
         * @param type Tipo de función objetivo.
         */
        void selectSites(int type);

        /**
         * Mueve a una abeja a un vecindario.
         *
         * @param a    Abeja.
         * @param type Tipo de función objetivo.
         */
        void neighbor(int a, int type);

        /**
         * Selecciona el mejor entre dos abejas.
         * 
         * @param i    Primera abeja.
         * @param j    Segunda abeja.
         * @param type Tipo de función objetivo.
         *
         * @return El índice de la mejor abeja.
         */
        int getBetter(int i, int j, int type);

        /**
         * Cantidad de abejas.
         */
        int I;

        /**
         * Indica que están haciendo las abejas.
         */
        int* bees;

        /**
         * Cantidad de sitios de búsqueda.
         */
        int m_sites;

        /**
         * Cantidad de sitios élite.
         */
        int e_sites;

        /**
         * Cantidad de sitios no-élite.
         */
        int o_sites;

        /**
         * Cantidad de abejas élite.
         */
        int e_bees;

        /**
         * Cantidad de abejas a otros sitios.
         */
        int o_bees;

        /**
         * Cantidad de repeticiones sin mejora.
         */
        int REPS;
};
#endif
