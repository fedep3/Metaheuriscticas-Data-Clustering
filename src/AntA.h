/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase concreta donde se definen las funciones y variables
 * necesarias para ejecutar el algoritmo hormiga.
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
         * Constructor de la clase AntA.
         *
         * @param _d      Datos del problema.
         * @param _m      Dimensión de cada dato.
         * @param _n      Cantidad de datos.
         * @param _k      Cantidad de clusters (iniciales).
         * @param _nA     Cantidad de hormigas.
         * @param _alpha2 Parametro usado para las probabilidades.
         * @param _it     Cantidad de iteraciones.
         * @param _met Métrica.
         */
        AntA(float** _d, int _m, int _n, int _nA, float _alpha2, 
                                                        int _it, int _met);
        
        /**
         * Destructor de la clase AntA.
         */
        ~AntA();


        /**
         * Inicializa las células donde las hormigas va a recoger y soltar
         * los pixeles. También hace que cada hormiga recoga un pixel.
         */
        virtual void initialize();

        /**
         * Ejecuta la metaheurística con los datos dados.
         *
         * @param type Si se utilizará una función objetivo de
         *             maximización o de minimización.
         */
        virtual void run(int type);

        /*
         * Calcula el parámetro alpha2 usado en las probabilidades.
         */
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

        /*
         * Funciones que devuelve el promedio de distancia entre el pixel y los
         * que se encuentra en la célula cell.
         * @param pixel Pixel que se quiere soltar.
         * @param cell Célula donde se quiere soltar.
         */
        float f(int pixel, int cell);
        
        /*
         * Calcula la probabilidad de dejar un pixel en la celula cell dada.
         * @param pixel Pixel que se quiere soltar.
         * @param cell Célula donde se quiere soltar.
         */
        float pdrop(int pixel, int cell);
        
        /*
         * Calcula la probabilidad agarrar un pixel en la célula cell.
         * @param pixel Pixel que se quiere agarrar.
         * @param cell Célula donde donde se encuentra.
         */
        float ppick(int pixel, int cell);

        /*
         * Agrega la célula cell a la memoria global en caso de no encontrarse.
         * @param Célula que se quiere agregar.
         */
        void addMemory(int cell);
        
        /*
         * Va a buscar los pixeles que no estan siendo cargados y la hormiga va a inten-
         * tar agarrarlo.
         * @param Número de la hormiga.
         */
        void pickAnt(int ra);
        
        /*
         * Se encarga de armar la solutción a partir de la células y las hormigas.
         */
        void reconstruct();

        /*
         * Procedimiento que se encarga de soltar el pixel de una hormiga. Funciona
         * del siguiente modo:
         *
         *    Si (Memoria global no esta llena)
         *      entonces 
         *          Si (memoria de la hormiga tiene por lo menos 1)
         *              entonces Revisa la memoria local de la hormiga y elige la mejor 
         *                       célula donde dejar, en caso que no logre soltar el
         *                       pixel intenta en una ceĺula aleatoria
         *              sino Dejar el pixel en una célula aleatoria
         *      sino
         *          Revisa la memoria global y elige la mejor célula donde dejar, en 
         *          caso que no logre soltar el pixel, intenta revisando la 
         *          memoria de la hormiga y si aún no lo logra, busca tratar
         *          de dejarlo en una célula aleatoria.
         *
         *    Finalmente busca agregar la célula donde soltó en la memoria.
         *
         * @param ra Número de la hormiga que quiere soltar el pixel.
         */
        void dropAnt(int ra);

        /*
         * Arreglo con los pixeles libres y en que célula se encuentra.
         */
        int* free;

        /*
         * Células donde las hormigas van a dejar y agarrar los píxeles.
         */
        vector<int> *cells;

        /*
         * Cantidad de hormigas.
         */
        int nA;

        /*
         * Número de iteraciones.
         */
        int maxit;
        
        /*
         * Parámetro usado para calcular f.
         */
        float alpha2;

        /*
         * Arreglo de las hormigas.
         */
        Ant* ants;

        /*
         * Booleano que indica si el parámetro alpha2 fue dado.
         */
        bool ac;

        /*
         * Arreglo para la memoria global.
         */
        int* globalMemory;

        /*
         * Tamaño actual de la memoria global.
         */
        int globalSize;

};
#endif
