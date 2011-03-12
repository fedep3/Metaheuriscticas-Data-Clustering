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
#ifndef _AntA_
#define _AntA_

#include <cstdio>
#include <cmath>
#include <utility>
#include <vector>
#include <fstream>
#include "Metaheuristic.h"
#include "Kmeans.h"
#include "Ant.h"

using namespace std;

#define PIH 1.570796327
#define GM 10

class AntA : public Metaheuristic{
    public:

        /**
         * Constructor de la clase AntA.
         *
         * @param _d      Datos del problema.
         * @param _m      Dimensión de cada dato.
         * @param _n      Cantidad de datos.
         * @param _k      Cantidad de clusters (iniciales).
         * @param _nAnt   Cantidad de hormigas.
         * @param _it     Cantidad de iteraciones.
         * @param _met Métrica.
         */
        AntA(float** _d, int _m, int _n, int _nAnt, int _it, int _met);
        
        /**
         * Destructor de la clase AntA.
         */
        ~AntA();


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

        void calcAlpha();


    private:
        
        /**
         * Intercambio la posición f y s, del arreglo dado.
         *
         * @param array Arreglo a intercambiar.
         * @param f Primero posición.
         * @param s Segundo posición.
         */
        inline void swap(int *array, int f, int s);

        float f(int pixel, int cell);
        
        float pdrop(int pixel, int cell);
        
        float ppick(int pixel, int cell);

        void addMemory(int cell);
        
        void pickAnt(int ra);
        
        void reconstruct();

        void dropAnt(int ra);

        int* free;

        vector<int> *cells;

        int nA;

        int maxit;
        
        float alpha2;

        Ant* ants;

        bool ac;

        int* globalMemory;

        int globalSize;

};
#endif
