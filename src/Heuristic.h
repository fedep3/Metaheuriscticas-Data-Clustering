/**
 * @copyright
 *
 * Project Athena for Data Clustering Metaheuristics focused on images.
 *
 * Copyright (C) 2011 Alexander De Sousa (alexanderjosedesousa@gmail.com),
 *                    Federico Ponte     (fedep3@gmail.com)
 *
 * This program is free software; you can redistribute it and/or modify it under 
 * the terms of the GNU General Public License as published by the Free Software 
 * Foundation; either version 2 of the License, or (at your option) any later 
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * @author Alexander De Sousa (alexanderjosedesousa@gmail.com),
 *         Federico Ponte     (fedep3@gmail.com)
 *
 * @section Description
 *
 * Definition of the abstract class Heuristic that contains the
 * basic methods for all the heuristics implemented in this project and its
 * basic exceptions.
 */
#include <cmath>
#include <ctime>
#include <limits>
#include <fcntl.h>
#include "BaseHeuristic.h"
#include "Data.h"
#include "mt.h"

#ifndef _HEURISTIC_
#define _HEURISTIC_

#define TURI_STD_DEVIATION  1
#define TURI_MEAN           2
#define TURI_C             25.0

using namespace std;

template <class IndividualT>
class Heuristic : public BaseHeuristic {
    public:
        /**
         * @param data Data to be clustered.
         * @param K    Maximum quantity of clusters in the final solution.
         * @param I    Quantity of individuals.
         */
        Heuristic(Data* data, int K, int I = 1);

        /**
         * Destructor.
         */
        virtual ~Heuristic();

        /**
         * Executes the heuristic.
         */
        virtual void run() = 0;

        /**
         * @return Best solution Davies-Bouldin validity index value.
         */
        float finalDB();

        /**
         * @return Best solution Turi validity index value.
         */
        float finalTuri();

        /**
         * @return Number of evaluations of the objective function.
         */
        int getNumberOfEvaluations();
    protected:
        /**
         * Data set information (patterns, quantity and size).
         */
        Data* data;

        /**
         * Max quantity of clusters.
         */
        int K;

        /**
         * Quantity of individuals.
         */
        int I;

        /**
         * Individuals of the algorithm.
         */
        IndividualT* individual;

        /**
          * Random number generator.
          */
        MTStore drand;
};

/**
 * @param data Data to be clustered.
 * @param K    Maximum quantity of clusters in the final solution.
 * @param I    Quantity of individuals.
 */
template <class IndividualT>
Heuristic<IndividualT>::Heuristic(Data* data, int K, int I) : BaseHeuristic() {
    int i;

    this->data = data;
    this->K    = K;
    this->I    = I;

    individual = new IndividualT[this->I];
    for(i = 0; i < this->I; ++i)
        individual[i].initialize(data, K);

    this->drand.mtRandomInit(&drand, time(NULL), K_2M31);
}

/**
 * Destructor.
 */
template <class IndividualT>
Heuristic<IndividualT>::~Heuristic(){
    delete [] individual;
}

/**
 * @return Best solution Davies-Bouldin validity index value.
 */
template <class IndividualT>
float Heuristic<IndividualT>::finalDB() {
    return bestIndividual.DB();
}

/**
 * @return Best solution Turi validity index value.
 */
template <class IndividualT>
float Heuristic<IndividualT>::finalTuri(){
    return bestIndividual.Turi();
}

/**
 * @return Number of evaluations of the objective function.
 */
template <class IndividualT>
int Heuristic<IndividualT>::getNumberOfEvaluations() {
    int i;
    int sum = 0;

    for(i = 0; i < I; ++i)
        sum += individual[i].evaluationOfDB;

    return sum;
}
#endif
