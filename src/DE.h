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
 * necesarias para ejecutar el diferential evolution.
 */
#ifndef _DE_
#define _DE_

#include <cstdio>
#include <time.h>
#include <vector>
#include "RandomArray.h"
#include "Metaheuristic.h"

using namespace std;

#define CrMIN 0.5
#define CrMAX 1.0


using namespace std;

class DE : public Metaheuristic{
    public:

        /**
         * Constructor de la clase DE.
         *
         * @param _d   Datos del problema.
         * @param _m   Dimensión de cada dato.
         * @param _n   Cantidad de datos.
         * @param _k   Cantidad de clusters (iniciales).
         * @param _i   Cantidad de individuos.
         * @param _it  Número de iteraciones.
         * @param _w1  Peso de la distancia de los elementos
         *             dentro de un cluster en la función
         *             objetivo.
         * @param _w2  Peso de la distancia entre clusters
         *             en la función objetivo.
         * @param _w3  Peso del error en la función objetivo.
         *             en la función objetivo.
         * @param _zmx Máximo valor de un atributo.
         * @param _zmn Mínimo valor de un atributo.
         * @param _met Métrica.
         */
        DE(float** _d, int _m, int _n, int _k, int _i, int _it, float _w1,
           float _w2, float _w3, float *_zmx, float *_zmn);

        /**
         * Constructor de la clase DE.
         *
         * @param _d   Datos del problema.
         * @param _m   Dimensión de cada dato.
         * @param _n   Cantidad de datos.
         * @param _k   Cantidad de clusters (iniciales).
         * @param _i   Cantidad de individuos.
         * @param _it  Número de iteraciones.
         * @param _Cr  Probabilidad de cruce.
         * @param _F   Escalar por el cual se multiplica la diferencia, es un 
         *             parámetro.
         * @param _w1  Peso de la distancia de los elementos
         *             dentro de un cluster en la función
         *             objetivo.
         * @param _w2  Peso de la distancia entre clusters
         *             en la función objetivo.
         * @param _w3  Peso del error en la función objetivo.
         *             en la función objetivo.
         * @param _zmx Máximo valor de un atributo.
         * @param _zmn Mínimo valor de un atributo.
         * @param _met Métrica.
         */
        DE(float** _d, int _m, int _n, int _k, int _i, int _it, float _Cr, 
           float _F, float _w1, float _w2, float _w3, float *_zmx, float *_zmn);

        /**
         * Inicializa los centroides de los clusters.
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
         * Reconstruye la solución.
         */
        virtual void reconstruct(int type);

        /**
         * Destructor de la clase GA.
         */
        ~DE();

    protected:

        /**
         * Reasigna los elementos a cada cluster en un individuo.
         *
         * @param solution Solucion.
         * @param centroid Centroide.
         */
        void assign(int *solution, float **centroid);

        /**
         * Obtiene el mejor de la población.
         *
         * @param type Tipo de función objetivo.
         * @return Número de individuo mejor de la población.
         */
        int getBetter(int type);

        /**
         * Función objetivo del algoritmo PSO.
         *
         * @param sol  Vector solución.
         * @param cent Centroides asociados al vector solución.
         * @param k    Cantidad de clusters en el vector solución.
         *
         * @return Valor de la función objetivo.
         */
        float deFO(int* sol, float** cent, int k);

        /**
         * Función objetivo de la metaheurística PSO.
         *
         * @param sol  Vector solución.
         * @param cent Centroides asociados al vector solución.
         * @param k    Cantidad de clusters.
         * @param met  Tipo de función objetivo.
         *
         * @return Valor de la solución con función objetivo
         *         indicada.
         */
        float foMin(int* sol, float** cent, int k);


        /**
         * Cantidad máxima de iteraciones
         */
        int maxit;

        /**
         * Cantidad de individuos.
         */
        int I;

        /**
         * Probabilidad de cruce.
         */
        float Cr;

        /**
         * Escalar a la hora de crear nuevos individuos.
         */
        float F;

        /**
         * Booleano para saber si los parámetros fueron dados, o son alea-
         * torios.
         */
        bool var;

        /**
         * Máxima distancia posible entre objetos.
         */
        float Zmax;

        /**
         * Peso de la distacia entre elementos de un cluster.
         */
        float w1;

        /**
         * Peso de la distancia entre clusters.
         */
        float w2;

        /**
         * Peso del error.
         */
        float w3;

        /**
         * Máximo valor de un atributo.
         */
        float *zmx;

        /**
         * Mínimo valor de un atributo.
         */
        float *zmn;

};
#endif
