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

#include "Kmeans.h"
/**
 * Constructor de la clase Kmeans.
 *
 * @param d   Datos del problema.
 * @param m   Dimensión de cada dato.
 * @param n   Cantidad de datos.
 * @param k   Cantidad de clusters (iniciales).
 * @param met Métrica.
 */
Kmeans::Kmeans(float** _d, int _m,  int _n, int _k, int _met, int _reps)
: Metaheuristic(_d, _m, _n, _k, _met){
    int i;
    //Sólo se necesita una solución.
    solution    = (int **) calloc(1, sizeof(int*));
    if(solution == NULL) exit(1);
    solution[0] = new int[N];

    //Sólo necesita un arreglo de centroides.
    centroid    = (float ***) calloc(1, sizeof(float**));
    if(centroid == NULL) exit(1);
    centroid[0] = (float**) calloc(K,sizeof(float*));
    if(centroid[0] == NULL) exit(1);
    for(i = 0; i < K; ++i){
        centroid[0][i] = new float[M];
    }

    of = new float[1];
    Ks = new int[1];

    Ks[0] = K;

    REPS = _reps;
}

/**
 * Destructor de la clase Kmeans.
 */
Kmeans::~Kmeans(){
    int i;
    delete [] solution[0];
    free(solution);

    for(i = 0; i < Kmax; ++i)
        delete [] centroid[0][i];
    free(centroid[0]);
    free(centroid);

    delete [] Ks;
    delete [] of;
}

/**
 * Ejecuta la metaheurística con los datos dados.
 *
 * @param type Si se utilizará una función objetivo de
 *             maximización o de minimización.
 */
void Kmeans::run(int type){
    int i;

    ////////////////////////////
    // Inicializaciones.

    //Inicializaciones del mejor según el tipo de problema.
    float best;
    if(initialized){
        switch(type){
            case T_MAX:
                //Maximización.
                best = 0.0;
                break;
            default:
                //Minimización.
                best = numeric_limits<float>::infinity();
        }
    }else{
        best = bestFO;
    }
    int count   = 0;

    ////////////////////////////
    // Algoritmo K-means.

    while(count < REPS){
        //////////////////////////////
        // Reasignación y evaluación.

        //Asignar elementos a cada cluster.
        for(i = 0; i < N; ++i)
            solution[0][i] = bestCluster(0, i);

        //Renombra los clusters y recalcula los centroides.
        renamer(0, &Ks[0], size);
        //Evalua la función objetivo.
        switch(type){
            case T_MAX:
                of[0] = foMax(0, Ks[0], metric);
                break;
            default:
                of[0] = foMin(0, Ks[0], metric);
        }

        ////////////////////////////
        // Actualización del mejor.
        updateBetter(0, &best, &count, type);
    }

}

/**
 * Inicializa los centroides de los clusters. No se utiliza si ya
 * se tienen centroides.
 */
void Kmeans::initialize(){

    int j, l, k;

    RandomArray rarr(N);

    for(j = 0; j < K; ++j){

        k = rarr.get();

        for(l = 0; l < M; ++l)
            centroid[0][j][l] = data[k][l];
    }

    initialized = true;
}

/**
 * Toma un vector de centroides e inicializa el arreglo
 * de centroides del K-means.
 *
 * @param cent Centroides. 
 */
void Kmeans::setCentroids(int* sol, float** cent, int type){
    int i, j;

    for(i = 0; i < K; ++i){
        for(j = 0; j < M; ++j){
            bestCentroids[i][j] = cent[i][j];
            centroid[0][i][j]      = cent[i][j];
        }
    }

    for(i = 0; i < N; ++i){
        bestSolution[i] = sol[i];
        solution[0][i]  = sol[i];
    }

    switch(type){
        case T_MAX:
            bestFO = foMax(0, K, metric);
            break;
        default:
            bestFO = foMin(0, K, metric);
    }

    initialized = false;
}
