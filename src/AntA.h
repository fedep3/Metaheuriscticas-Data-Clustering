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
 * necesarias para ejecutar el algoritmo hormiga.
 */
#ifndef _AntA_
#define _AntA_

#include <cmath>
#include <vector>
#include "RandomArray.h"
#include "Metaheuristic.h"
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
         * @param _nAnt   Cantidad de hormigas.
         * @param _it     Cantidad de iteraciones.
         * @param _met    Métrica.
         */
        AntA(float** _d, int _m, int _n, int _nAnt, int _it, int _met);

        /**
         * Constructor de la clase AntA.
         *
         * @param _d      Datos del problema.
         * @param _m      Dimensión de cada dato.
         * @param _n      Cantidad de datos.
         * @param _nA     Cantidad de hormigas.
         * @param _alpha2 Parametro usado para las probabilidades.
         * @param _it     Cantidad de iteraciones.
         * @param _met    Métrica.
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
         * Se encarga de armar la solutción a partir de la células y las 
         * hormigas.
         *
         * @param type Si se utilizará una función objetivo de
         *             maximización o de minimización.
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
        
        /*
         * Funciones que devuelve el promedio de distancia entre el pixel y los
         * que se encuentran en la célula cell.
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
         * Va a buscar los pixeles que no estan siendo cargados y la hormiga va
         * a intentar agarrar uno.
         *
         * @param Número de la hormiga.
         */
        void pickAnt(int ra);
        

        /*
         * Procedimiento que se encarga de soltar el pixel de una hormiga. Funciona
         * del siguiente modo:
         *
         * Si (memoria tiene por lo menos 1) entonces
         *      Revisa la memoria y elige la mejor célula donde dejar,
         *      en caso que no logre soltar el
         *      pixel intenta en una ceĺula aleatoria.
         * sino Dejar el pixel en una célula aleatoria
         *
         * @param ra Número de la hormiga que quiere soltar el pixel.
         */
        void dropAnt(int ra);

        /*
         * Agrega a la memoria la celula dada, si ya existe simplemente 
         * cuenta que dejo una vez ahi. Cada cierto numero de iteraciones
         * vacia un elemento de la memoria en la cual se ha dejado menos veces.
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
