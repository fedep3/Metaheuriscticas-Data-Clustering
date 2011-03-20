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
        void setCentroids(float** cent);

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
         * Reconstruye la solución.
         */
        virtual void reconstruct(int type);

        /**
         * Destructor de la clase Kmeans.
         */
        ~Kmeans();
    protected:
        /**
         * Cantidad de repeticiones sin mejora.
         */
        int REPS;
};
#endif
