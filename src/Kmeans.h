/*
Data Clustering Metaheuristics focused on images.
Copyright (C) 2011 Alexander De Sousa(prof.etadel2@gmail.com), 
                                                Federico Ponte(fedep3@gmail.com)

This program is free software; you can redistribute it and/or modify it under 
the terms of the GNU General Public License as published by the Free Software 
Foundation; either version 2 of the License, or (at your option) any later 
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY 
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
PARTICULAR PURPOSE. See the GNU General Public License for more details.
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
