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
        DE(float** _d, int _m, int _n, int _k, int _i, int _it, float *_zmx, 
                                                       float *_zmn, int _met);

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
                               float _F, float *_zmx, float *_zmn, int _met);

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
         * Destructor de la clase GA.
         */
        ~DE();

    protected:

        /**
         * Busca el mejor cluster para un objeto, tomando en cuanta los centroi-
         * des activados.
         *
         * @param centroid Centroides del individuo.
         * @param activation Arreglo con los valores de activación de los centroides.
         * @param e Objeto.
         *
         * @return Mejor cluster para el objeto.
         */
        int bestCluster( float **centroid, int e);

        /**
         * Intercambio la posición f y s, del arreglo dado.
         *
         * @param array Arreglo a intercambiar.
         * @param f Primero posición.
         * @param s Segundo posición.
         */
        void swap(int *array, int f, int s);

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

        float Zmax;

};
#endif
