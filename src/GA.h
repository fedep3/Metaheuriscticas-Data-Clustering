/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase concreta donde se definen las funciones y variables
 * necesarias para ejecutar el algoritmo genético.
 */
#include <cstdio>
#include <time.h>
#include "Metaheuristic.h"

#ifndef _GA_
#define _GA_

using namespace std;

class GA : public Metaheuristic{
    public:

        /**
         * Constructor de la clase GA.
         *
         * @param _d   Datos del problema.
         * @param _m   Dimensión de cada dato.
         * @param _n   Cantidad de datos.
         * @param _k   Cantidad de clusters (iniciales).
         * @param _i   Cantidad de individuos.
         * @param _cp  Porcentaje de cruce.
         * @param _mut Porcentaje de mutación.
         * @param _met Métrica.
         * @param _reps Repeticiones sin mejora.
         */
        GA(float** _d, int _m, int _n, int _k, int _i,
           float _cp, float _mut, int _ts,
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
         * Reconstruye la solución.
         */
        virtual void reconstruct(int type);

        /**
         * Destructor de la clase GA.
         */
        ~GA();

    protected:
        /**
         * Selección de padres que se cruzarán.
         *
         * @param p1   Número de individuo del primer padre.
         * @param p2   Número de individuo del segundo padre.
         * @param type Tipo de funciones objetivo.
         */
        void selection(int* p1, int* p2, int type);

        /**
         * Realiza un torneo para elegir a un padre.
         *
         * @param p    Número del padre elegido.
         * @param type Tipo de funciones objetivo.
         */
        void torneo(int* p, int type);

        /**
         * Cruce de los padres. Los sustituye en caso de obtener
         * mejores hijos.
         *
         * @param p1   Número de individuo del primer padre.
         * @param p2   Número de individuo del segundo padre.
         * @param type Tipo de función objetivo.
         */
        void crossover(int p1, int p2, int type);

        /**
         * Mutación de algún individuo de la población.
         *
         * @param type Tipo de función objetivo.
         */
        void mutation(int type);

        /**
         * Cambia un gen de un cluster a otro.
         *
         * @param i Número del individuo.
        */
        void crandom(int i);

        /**
         * En el individuo i, se elige un cluster y se cambian
         * la mitad de sus elementos a uno nuevo.
         *
         * @param i Número del individuo.
        */
        void split(int i);

        /**
         * Cuenta la cantidad de elementos del cluster c en el
         * individuo i.
         *
         * @param i Número del individuo.
         * @param c Cluster a contar.
         * @return Cantidad de elementos pertenecientes al cluster.
        */
        int count( int i, int c );

        /**
         * Une dos clusters aleatorios.
         *
         * @param i Número del individuo.
        */
        void merge(int i);

        /**
         * Obtiene el mejor de la población.
         *
         * @param type Tipo de función objetivo.
         * @return Número de individuo mejor de la población.
         */
        int getBetter(int type);

        /**
         * Cantidad de individuos.
         */
        int I;

        /**
         * Porcentaje de cruce.
         */
        float CP;

        /**
         * Porcentaje de mutación.
         */
        float MUT;

        /**
         * Tamaño del torneo en la selección.
         */
        int torneoSize;

        /**
         * Primer hijo.
         */
        int* s1;

        /**
         * Centroides de los hijos.
         */
        float** centroidS1;

        /**
         * Segundo hijo.
         */
        int* s2;

        /**
         * Centroides del segundo hijo.
         */
        float** centroidS2;

        /**
         * Cantidad de repeticiones sin mejora.
         */
        int REPS;
};
#endif
