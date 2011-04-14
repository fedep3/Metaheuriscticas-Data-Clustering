/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase concreta donde se definen las funciones y variables
 * necesarias para ejecutar el algoritmo de particle swarm
 * optimization (PSO).
 */
#include <cstdio>
#include <time.h>
#include "Metaheuristic.h"

#ifndef _PSO_
#define _PSO_

#define F_NON_WEIGHTED 0
#define F_WEIGHTED     1

using namespace std;

class PSO : public Metaheuristic{
    public:

        /**
         * Constructor de la clase PSO.
         *
         * @param _d   Datos del problema.
         * @param _m   Dimensión de cada dato.
         * @param _n   Cantidad de datos.
         * @param _k   Cantidad de clusters (iniciales).
         * @param _p   Cantidad de partículas.
         * @param _c1  Constante de la componente cognitiva
         *             de las partículas.
         * @param _c2  Constante de la componente social
         *             de las partículas.
         * @param _w   Peso inercial de las partículas.
         * @param _w1  Peso de la distancia de los elementos
         *             dentro de un cluster en la función
         *             objetivo.
         * @param _w2  Peso de la distancia entre clusters
         *             en la función objetivo.
         * @param _w3  Peso del error en la función objetivo.
         *             en la función objetivo.
         * @param _zmx Máximo valor de un atributo.
         * @param _zmn Mínimo valor de un atributo.
         * @param _vmx Velocidad máxima.
         * @param _fun Tipo de función objetivo.
         * @param _reps Repeticiones sin mejora.
         */
        PSO(double** _d, int _m, int _n, int _k, int _p,
                 double _c1, double _c2, int _w,
                 double _w1, double _w2, double _w3,
                 double* _zmx, double* _zmn, double _vmx,
                 int _fun, int _reps);

        /**
         * Constructor de la clase PSO.
         *
         * @param _d   Datos del problema.
         * @param _m   Dimensión de cada dato.
         * @param _n   Cantidad de datos.
         * @param _k   Cantidad de clusters (iniciales).
         * @param _p   Cantidad de partículas.
         * @param _c1  Constante de la componente cognitiva
         *             de las partículas.
         * @param _c2  Constante de la componente social
         *             de las partículas.
         * @param _w   Peso inercial de las partículas.
         * @param _zmx Máximo valor de un atributo.
         * @param _zmn Mínimo valor de un atributo.
         * @param _vmx Velocidad máxima.
         * @param _fun Tipo de función objetivo.
         * @param _reps Repeticiones sin mejora.
         */
        PSO(double** _d, int _m, int _n, int _k, int _p,
                 double _c1, double _c2, int _w,
                 double* _zmx, double* _zmn, double _vmx,
                 int _fun, int _reps);

        /**
         * Inicializa la población. (Soluciones, centroides y
         * tamaños de clusters.
         */
        virtual void initialize();

        /**
         * Ejecuta la metaheurística con los datos dados.
         *
         * @param type Que tipo de función objetivo se utilizará.
         */
        virtual void run(int type);

        /**
         * Destructor de la clase GA.
         */
        ~PSO();

    protected:
        /**
         * Actualiza la velocidad de la partícula.
         *
         * @param p Partícula.
         */
        void updateVelocity(int p);

        /**
         * Actualiza una partícula de acuerdo a su velocidad.
         *
         * @param p Partícula.
         */
        void updateParticle(int p);

        /**
         * Actualiza la mejor posición de la partícula.
         *
         * @param p  Partícula.
         * @param fo Valor de la función objetivo.
         */
        void updateBestParticle(int p, double fo);

        /**
         * Actualiza el mejor global de la población.
         *
         * @param p  Partícula.
         * @param fo Valor de la función objetivo.
         */
        bool updateBest(int p, double fo);

        /**
         * Reasigna los elementos a cada cluster en una partícula.
         *
         * @param p Párticula.
         */
        void assign(int p);

        /**
         * Función objetivo de la metaheurística PSO.
         *
         * @param i   Individuo.
         * @param k   Cantidad de clusters.
         * @param met Tipo de función objetivo.
         *
         * @return Valor de la solución con función objetivo
         *         indicada.
         */
        virtual double foMin(int i, int k, int fun);

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
        virtual double foMin(int* sol, double** cent, int k, int fun);

        /**
         * Función objetivo del algoritmo PSO.
         *
         * @param sol  Vector solución.
         * @param cent Centroides asociados al vector solución.
         * @param k    Cantidad de clusters en el vector solución.
         *
         * @return Valor de la función objetivo.
         */
        double psoFO(int* sol, double** cent, int k);

        /**
         * Cantidad de partículas.
         */
        int P;

        /**
         * Velocidad de las partículas.
         */
        double*** velocity;

        /**
         * Mejor posición de la partícula.
         */
        double*** bestParticle;

        /**
         * Constante de la componente cognitiva de la partícula.
         * Mientras más grande, más importante es su pasado.
         * Valor recomendado de 1.49 para asegurar convergencia
         * [Van den Bergh 2002].
         * 
         */
        double c1;

        /**
         * Constante de la componente social de la partícula.
         * Mientras más grande, más importante es la sociedad.
         * Valor recomendado de 1.49 para asegurar convergencia
         * [Van den Bergh 2002].
         */
        double c2;

        /**
         * Peso inercial de partícula.
         * Mientras más grande, tiende a diversificarse.
         * Valor recomendado de 0.72 para asegurar convergencia
         * [Van den Bergh 2002].
         */
        double W;

        /**
         * Máxima velocidad de las partículas.
         */
        double Vmax;

        ///////////////////////////////////////
        // Variables de la función objetivo.

        /**
         * Peso de la distacia entre elementos de un cluster.
         */
        double w1;

        /**
         * Peso de la distancia entre clusters.
         */
        double w2;

        /**
         * Peso del error.
         */
        double w3;

        /**
         * Máxima distancia posible entre objetos.
         */
        double Zmax;

        /**
         * Cantidad de repeticiones sin mejora.
         */
        int REPS;
};
#endif
