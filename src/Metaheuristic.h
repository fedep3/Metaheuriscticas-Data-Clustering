/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase abstracta en donde se definen las funciones y variables
 * básicas de una metaheurística.
 */
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <vector>

#ifndef _METAHEURISTIC_
#define _METAHEURISTIC_

//Qué tipo de distancia será usada.
#define COSINE_DISTANCE    0
#define EUCLIDEAN_DISTANCE 1
#define DEFAULT_DISTANCE   1

//Si el problema es de maximización o minimización.
#define T_MAX 0
#define T_MIN 1

//Qué métrica usará.
#define M_DB 0
#define M_CS 1
#define MIN_DIST 0.00001

using namespace std;

class Metaheuristic{
    public:
        /**
         * Constructor de la clase Metaheuristic.
         *
         * @param d Datos del problema.
         * @param m Dimensión de cada dato.
         * @param n Cantidad de datos.
         * @param k Cantidad de clusters (iniciales).
         */
        Metaheuristic(float** _d, int _m, int _n, int _k, int _met);

        /**
         * Ejecuta la metaheurística con los datos dados.
         */
        virtual void run(int type) = 0;

        /**
         * Inicializa los centroides de los clusters.
         */
        virtual void initialize() = 0;

        /**
         * Reconstruye la solución.
         */
        virtual void reconstruct(int type);

        /**
         * Calcula 1/DB()
         */
        virtual void calcGFO();

        /**
         * Destructor de la clase Metaheuristic.
         */
        virtual ~Metaheuristic();

        /**
         * Mejor solución. |solution[_]| == |bestSolution|
         */
        int* bestSolution;

        /**
         * Mejores centroides.
         */
        float** bestCentroids;

        /**
         * Mejor valor de la función objetivo.
         */
        float bestFO;

        /**
         * Cantidad de clusters.
         */
        int K;

        /**
         * Mejor valor heurístico usando la métrica DB.
         */
        float bestDB;

        /**
         * Cantidad de datos.
         */
        int N;

        /**
         * Dimensión de cada dato.
         */
        int M;

        /**
         * Datos.
         */
        float** data;

        /**
         * Cantidad de evaluación.
         */
        int ofEval;

    protected:
        /**
         * Calcula la distancia entre dos objetos. 
         *
         * @param i Primer objeto.
         * @param j Segundo objeto.
         *
         * @return Distancia entre los objetos.
         */
        float d(int i, int j);

        /**
         * Distancia entre dos vectores cualesquiera.
         *
         * @param v1 Primer vector.
         * @param v2 Segundo vector.
         *
         * @return Distancia entre los vectores.
         */
        float d(float* v1, float* v2);

        /**
         * Calcula la distancia del coseno entre dos vectores.
         *
         * @param v1 Primer vector.
         * @param v2 Segundo vector.
         *
         * @return Distancia del coseno entre los dos vectores
         *         dados.
         */
        float cosineDistance(float* v1, float* v2);

        /**
         * Calcula la distancia euclideana entre dos vectores.
         * El cáculo debería hacerse con q = 2.0 de modo que se
         * calcule la distancia euclideana. Sin embargo, se puede
         * usar cualquier otra potencia.
         *
         * @param v1 Primer vector.
         * @param v2 Segundo vector.
         *
         * @return Distancia euclideana entre los dos vectores
         *         dados.
         */
        float euclideanDistance(float* v1, float* v2, float q = 2.0);

        /**
         * Calcula la norma de un vector. Para calcular la norma
         * cuadrada q = 2.0.
         *
         * @param vec Vector.
         * @param dim Dimensión del vector.
         * @param q   Exponente de la norma.
         *
         * @return Norma del vector.
         */
        float norm(float* vec, int dim, float q = 2.0);

        /**
         * Busca el mejor cluster para un objeto.
         *
         * @param i Individuo.
         * @param e Objeto.
         *
         * @return Mejor cluster para el objeto.
         */
        int bestCluster(int i, int e);

        /**
         * Busca el mejor cluster para un objeto.
         *
         * @param solution Solución.
         * @param centroids Centroides.
         * @param e Objeto.
         *
         * @return Mejor cluster para el objeto.
         */
        int bestCluster(float **centroid, int e);

        /**
         * Métrica DB. A menor valor de DB, mejor es la solución.
         *
         * @param i Individuo.
         * @param k Cantidad de clusters.
         *
         * @return Valor de métrica.
         */
        float DB(int i, int k);

        /**
         * Métrica DB. A menor valor de DB, mejor es la solución.
         *
         * @param sol  Vector solución.
         * @param cent Centroides asociados al vector solución.
         * @param k    Cantidad de clusters en el vector solución.
         *
         * @return Valor de métrica.
         */
        float DB(int* sol, float** cent, int k);

        /**
         * Métrica CS. A menor valor de CS, mejor es la solución.
         *
         * @param i Individuo.
         * @param k Cantidad de clusters.
         *
         * @return Valor de métrica.
         */
        float CS(int i, int k);

        /**
         * Métrica CS. A menor valor de CS, mejor es la solución.
         *
         * @param sol  Vector solución.
         * @param cent Centroides asociados al vector solución.
         * @param k    Cantidad de clusters en el vector solución.
         *
         * @return Valor de métrica.
         */
        float CS(int* sol, float** cent, int k);

        /**
         * Utiliza minimización de la métrica indicada. Si la métrica no
         * aparece, entonces retorna 0.0.
         *
         * @param i   Individuo.
         * @param k   Cantidad de clusters.
         * @param met Métrica.
         *
         * @return Valor de la solución con la métrica indicada.
         */
        virtual float foMin(int i, int k, int met);

        /**
         * Utiliza minimización de la métrica indicada. Si la métrica no
         * aparece, entonces retorna 0.0.
         *
         * @param sol  Vector solución.
         * @param cent Centroides asociados al vector solución.
         * @param k    Cantidad de clusters.
         * @param met  Métrica.
         *
         * @return Valor de la solución con la métrica indicada.
         */
        virtual float foMin(int* sol, float** cent, int k, int met);

        /**
         * Utiliza maximización de la métrica indicada. Si la métrica no
         * aparece, entonces retorna 0.0.
         *
         * @param i   Individuo.
         * @param k   Cantidad de clusters.
         * @param met Métrica.
         *
         * @return Valor de la solución con la métrica indicada.
         */
        virtual float foMax(int i, int k, int met);

        /**
         * Utiliza maximización de la métrica indicada. Si la métrica no
         * aparece, entonces retorna 0.0.
         *
         * @param sol  Vector solución.
         * @param cent Centroides asociados al vector solución.
         * @param k    Cantidad de clusters.
         * @param met  Métrica.
         *
         * @return Valor de la solución con la métrica indicada.
         */
        virtual float foMax(int* sol, float** cent, int k, int met);

        /**
         * Reasigna los objetos a los centroides.
         *
         * @param sol   Vector solución.
         * @param cent  Centroides.
         * @param k     Cantidad de clusters.
         */
        void reassign(int* sol, float** cent, int k);

        /**
         * Renombra los elementos de un vector.
         *
         * @param i Individuo.
         * @param k Cantidad de clusters del mismo.
         */
        void renamer(int i, int* k);

        /**
         * Renombra los elementos de un vector.
         *
         * @param sol Vector solución.
         * @param k   Cantidad de clusters del mismo.
         */
        void renamer(int* sol, int* k);

        /**
         * Renombra los elementos de un vector y calcula, además,
         * los centroides.
         *
         * @param i    Individuo.
         * @param k    Cantidad de clusters del mismo.
         * @param size Arreglo de tamaño k.
         *
         * @return Devuelve la cantidad de clusters vacíos
         *         removidos.
         */
        int renamer(int i, int* k, int* size);

        /**
         * Renombra los elementos de un vector y sus centroides.
         *
         * @param sol  Vector solución.
         * @param cent Centroides para el vector solución.
         * @param k    Cantidad de clusters del mismo.
         *
         */
        void renamer(int* sol, float **cent, int *k);

        /**
         * Renombra los elementos de un vector y calcula, además,
         * los centroides.
         *
         * @param sol  Vector solución.
         * @param cent Centroides para el vector solución.
         * @param k    Cantidad de clusters del mismo.
         * @param size Arreglo de tamaño k.
         *
         * @return Devuelve la cantidad de clusters vacíos
         *         removidos.
         */
        int renamer(int* sol, float** cent, int* k, int* size);

        /**
         * Actualiza el mejor encontrado en la población.
         *
         * @param i     Mejor individuo de la población.
         * @param best  Mejor valor heurístico de la función
         *              objetivo.
         * @param last  Último valor de la función objetivo.
         * @param count Cantidad de veces que se ha repetido una
         *              solución en las últimas iteraciones.
         * @param type  Tipo de función objetivo.
         */
        void updateBetter(int i, float* best, float* last, int* count, int type);

        /**
         * Cantidad de clusters inicial.
         */
        int Kmax;

        /**
         * Métrica a usar.
         */
        int metric;

        /**
         * (Mejor) solución.
         */
        int** solution;

        /**
         * Centroides.
         */
        float*** centroid;

        /**
         * Funciones objetivo de la población.
         */
        float* of;

        /**
         * Cantidad de clusters de la población.
         */
        int* Ks;

        /**
         * Arreglo necesario para el recálculo de centroides.
         */
        int* size;
}; 
#endif
