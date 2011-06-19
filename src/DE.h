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
         * @param _met Métrica.
         */
        DE(float** _d, int _m, int _n, int _k, int _i, int _it, float _Cr, 
           float _F, float _w1, float _w2, float _w3, float *_zmx, float *_zmn);

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
         * Reconstruye la solución.
         */
        virtual void reconstruct(int type);

        /**
         * Destructor de la clase GA.
         */
        ~DE();

    protected:

        /**
         * Intercambio la posición f y s, del arreglo dado.
         *
         * @param array Arreglo a intercambiar.
         * @param f Primero posición.
         * @param s Segundo posición.
         */
        void swap(int *array, int f, int s);

        /**
         * Reasigna los elementos a cada cluster en una partícula.
         *
         * @param solution Solucion.
         * @param centroid Centroide.
         */
        void assign(int *solution, float **centroid);

        /**
         * Cuenta el número de clusters diferentes a los que pertenece un
         * datos.
         * @param solution Arreglo con la solución.
         * @return Cantidad de clusters diferentes.
         */
        int countClust(int *solution);

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

};
#endif
