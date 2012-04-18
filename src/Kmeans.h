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
 * Concrete class where it's defined the K-means algorithm for numerical data
 * clustering.
 */
#include "Heuristic.h"
#include "RandomArray.h"

#ifndef _KMEANS_
#define _KMEANS_

using namespace std;

class Kmeans : public Heuristic<Individual>{
    public:

        /**
         * @param data             Data to be clustered.
         * @param K                Maximum quantity of clusters in the final solution.
         * @param improvementLevel Improvement level of the solution.
         */
        Kmeans(Data* data, int K, int improvementLevel);

        /**
         * Destructor.
         */
        ~Kmeans();

        /**
         * Executes the K-means algorithm.
         */
        void run();

        /**
         * Takes an extern individual as its own.
         *
         * @param externIndividual Individual.
         */
        void setCentroids(Individual& externIndividual);
    protected:
        /**
         * Improvement level of the algorithm.
         */
        int improvementLevel;
};
#endif
