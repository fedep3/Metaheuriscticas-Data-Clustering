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
#define MM 10
#define MIT 100

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
         * Ejecuta la metaheurística con los datos dados. Además al final
         * de ella ejecuta un K-means, ya que la imagen que genera tiene
         * mucho ruido.
         *
         * @param type Si se utilizará una función objetivo de
         *             maximización o de minimización.
         */
        virtual void run(int type);

        /*
         * Se encarga de armar la solutción a partir de la células y las hormigas.
         */
        virtual void reconstruct(int type);

        /**
         * Calcula 1/DB()
         */
        virtual void calcGFO();

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
         *
         * @param pixel Pixel que se quiere soltar.
         * @param cell Célula donde se quiere soltar.
         */
        float f(int pixel, int cell);
        
        /*
         * Calcula la probabilidad de dejar un pixel en la celula cell dada.
         *
         * @param pixel Pixel que se quiere soltar.
         * @param cell Célula donde se quiere soltar.
         */
        float pdrop(int pixel, int cell);
        
        /*
         * Calcula la probabilidad agarrar un pixel en la célula cell.
         *
         * @param pixel Pixel que se quiere agarrar.
         * @param cell Célula donde donde se encuentra.
         */
        float ppick(int pixel, int cell);

        /*
         * Va a buscar los pixeles que no estan siendo cargados y la hormiga va a inten-
         * tar agarrarlo.
         *
         * @param Número de la hormiga.
         */
        void pickAnt(int ra);
        
        /*
         * Procedimiento que se encarga de soltar el pixel de una hormiga. Funciona
         * del siguiente modo:
         *
         * Si (memoria de la hormiga tiene por lo menos 1)
         *     entonces Revisa la memoria local de la hormiga y elige la mejor 
         *         célula donde dejar, en caso que no logre soltar el
         *         pixel intenta en una ceĺula aleatoria
         * sino Dejar el pixel en una célula aleatoria
         *
         * @param ra Número de la hormiga que quiere soltar el pixel.
         */
        void dropAnt(int ra);

        /*
         * Agrega a la memoria la celula dada, si ya existe simplemente 
         * cuenta que dejo una vez ahi. Cada cierto numero de iteraciones
         * vacia un elemento de la memoria.
         *
         * @param cell Celula donde se solto el pixel.
         */
        void addMemory(int cell);

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
         * Tamano del arreglo de celulas.
         */
        int cellsSize;

        /*
         * Parámetro usado en la función renconstruct.
         */
        float alpha;

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
         * Arreglo de tamano MM, que se usara como la memoria.
         */
        int *memory;

        /*
         * Es para contar cuantas veces se ha dejado en un casilla de la 
         * memoria.
         */
        int *mcount;

        /*
         * Tamano de la memoria.
         */
        int msize;

        /*
         * Numero de iteraciones que se ha agregado en la memoria.
         */
        int mit;

};
#endif
