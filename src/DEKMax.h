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
#ifndef _DEKmax_
#define _DEKmax_

#include <cstdio>
#include <time.h>
#include <vector>
#include "Metaheuristic.h"

using namespace std;

#define CrMIN 0.5
#define CrMAX 1.0


using namespace std;

class DEKMax : public Metaheuristic{
    public:

        /**
         * Constructor de la clase DEKMax.
         *
         * @param _d   Datos del problema.
         * @param _m   Dimensión de cada dato.
         * @param _n   Cantidad de datos.
         * @param _k   Cantidad de clusters (iniciales).
         * @param _i   Cantidad de individuos.
         * @param _it  Número de iteraciones.
         * @param _met Métrica.
         */
        DEKMax(float** _d, int _m, int _n, int _k, int _i, int _it, int _met);

        /**
         * Constructor de la clase DEKMax.
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
        DEKMax(float** _d, int _m, int _n, int _k, int _i, int _it, float _Cr, 
                                                       float _F, int _met);

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
        ~DEKMax();

    protected:

        /**
         * Busca el mejor cluster para un objeto, tomando en cuanta los centroi-
         * des activados.
         *
         * @param i Individuo.
         * @param e Objeto.
         *
         * @return Mejor cluster para el objeto.
         */
        int bestCluster(int i, int e);

        /**
         * Va a contar el número de centroides activados y va a devolver un
         * vector con la posiciones de los centroides activados, y el n
         * se coloca el número total de ellos.
         *
         * @param activation Arreglo que contiene los valores de activación.
         * @param n Entero donde se va a colocar la cantidad de centroides
         *          activos.
         *
         * @return Vector con las posiciones de los centroides activos.
         */
        vector<int> *centsOn(float *activation, int &n);

        /**
         * Cuenta el número de clusters diferentes a los que pertenece un
         * datos.
         *
         * @param solution Arreglo con la solución.
         *
         * @return Cantidad de clusters diferentes.
         */
        int countClust(int *solution);

        /**
         * Utiliza minimización de la métrica indicada. Si la métrica no
         * aparece, entonces retorna 0.0.
         *
         * @param sol  Vector solución.
         * @param cent Centroides asociados al vector solución.
         * @param activation Arreglo de los centroides activados.
         * @param met  Métrica.
         *
         * @return Valor de la solución con la métrica indicada.
         */
        float foMin(int* sol, float** cent, float* activation, int met);

        /**
         * Utiliza maximización de la métrica indicada. Si la métrica no
         * aparece, entonces retorna 0.0.
         *
         * @param sol  Vector solución.
         * @param cent Centroides asociados al vector solución.
         * @param activation Arreglo de los centroides activados.
         * @param met  Métrica.
         *
         * @return Valor de la solución con la métrica indicada.
         */
        float foMax(int* sol, float** cent, float* activation, int met);

        /**
         * Métrica DB. A menor valor de DB, mejor es la solución. Sólo va a tomar
         * en cuenta los centroides activados.
         *
         * @param sol  Vector solución.
         * @param cent Centroides asociados al vector solución.
         * @param activation Arreglo de los centroides activados.
         *
         * @return Valor de métrica.
         */
        float DB(int* sol, float** cent, float* activation);

        /**
         * Métrica CS. A menor valor de CS, mejor es la solución. Sólo va a tomar
         * en cuenta los centroides activados.
         *
         * @param sol  Vector solución.
         * @param cent Centroides asociados al vector solución.
         * @param activation Arreglo de los centroides activados.
         *
         * @return Valor de métrica.
         */
        float CS(int* sol, float** cent, float* activation);

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
        int bestCluster( float **centroid, float* activation, int e);

        /** Devuelve un booleano dependiendo si el valor de activacion e es mayor a 
          * 0.5.
          *
          * @param activation Arreglo con los valores de activación.
          * @param e Posición del valor.
          *
          * @ return booleano true si es mayor a 0.5, sino falso.
          */
        bool activated(float* activation, int e);

        /**
         * Intercambio la posición f y s, del arreglo dado.
         *
         * @param array Arreglo a intercambiar.
         * @param f Primero posición.
         * @param s Segundo posición.
         */
        void swap(int *array, int f, int s);

        /**
         * Esta fución va a hacer dos cosas: 
         * 1.- Si se tienen menos de dos clusters, va hacer que halla dos valores de
         * activacion mayores que 0.5 y va a cambiar la solución colocando estos dos 
         * valores de forma aleatoria.
         * 2.- En el caso que halla cluster con menos de dos elementos, va a reiniciar 
         * toda la solucón de nuevo colocanto N/k elementos aleatorios en cada clusters.
         * donde N es la catidad total de datos y k la cantidad de clusters activos.
         * Finalmente si ocurrió la 1, 2 o ambas, se recalculan  los centroides.
         *
         * @param solution Arreglo solución.
         * @param centroid Matriz con cada uno de los centroides.
         * @param activation Arreglo con los valores de activación.
         */
        void stabilize(int *solution, float** centroid, float *activation);

        /** Cuenta la cantidad de elementos de [0...Kmax] clusters. Retorna un arreglo
          * con la cantidad de cada uno.
          *
          * @param solution Arreglo solución.
          *
          * @return Arreglo con la cantidad de cada uno de los diversos clusters.
          */
        int *countE(int *solution);

        /**
         * Obtiene el mejor de la población.
         *
         * @param type Tipo de función objetivo.
         * @return Número de individuo mejor de la población.
         */
        int getBetter(int type);


        /**
         * Cantidad máxima de iteraciones
         */
        int maxit;

        /**
         * Cantidad de individuos.
         */
        int I;

        /**
         * Matriz con los factores de activación de cada individuo.
         */ 
        float **activation;

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

};
#endif
