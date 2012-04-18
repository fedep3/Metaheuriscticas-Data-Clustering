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
 * Definition of an individual (solution).
 */
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <limits>
#include <map>
#include "RandomArray.h"
#include "Data.h"
#include "mt.h"

#ifndef _INDIVIDUAL_
#define _INDIVIDUAL_

#define TURI_STD_DEVIATION  1
#define TURI_MEAN           2
#define TURI_C             25.0

using namespace std;

class ClusterSizeTooLowException: public exception {
    public:
        ClusterSizeTooLowException() { }

        /**
         * Explanatory message of the exception.
         */
        virtual const char* what() const throw() {
            return "ClusterSizeTooLowException: Cluster size is either 0 or 1. This should be already filtered.";
        }
};

class Individual {
    public:
        Individual();

        /**
         * @param data Data to be clustered.
         * @param K    Maximum quantity of clusters in the final solution.
         */
        Individual(Data* data, int K);

        /**
         * @param data Data to be clustered.
         * @param K    Maximum quantity of clusters in the final solution.
         */
        void initialize(Data* data, int K);

        /**
         * Destructor.
         */
        virtual ~Individual();

        /**
         * Copy constructor.
         */
        Individual(const Individual& individual);

        /**
         * Assign operator.
         */
        Individual & operator=(const Individual & individual);

        /**
         * Swaps individuals.
         *
         * @param individual Individual to be swaped.
         */
        void swap(Individual & individual);

        /**
         * @return Whether the individual is better than other or not.
         */
        bool isBetterThan(Individual & individual);

        /**
         * Rearrange the solution.
         */
        void rearrange();

        /**
         * @return Davies-Bouldin index value for the solution.
         */
        float DB();

        /**
         * @return Turi validity index for the solution.
         */
        float Turi();

        /**
         * Centroids.
         */
        float** centroid;

        /**
         * Solution.
         */
        int* solution;

        /**
         * Index value.
         */
        double indexValue;

        /**
         * Number of times the DB index is evaluated in the algorithm.
         */
        int evaluationOfDB;

        /**
         * Quantity of clusters.
         */
        int K;

    protected:
        /**
         * @param patternID Pattern identificator.
         *
         * @return Index for the best cluster for the pattern.
         */
        int getBestClusterIndex(int patternID);

        /**
         * Reassign the patterns to the best cluster for them.
         */
        void reassign();

        /**
         * Re-labels the clusters identificators in the solution.
         */
        void relabel();

        /**
         * Recalculate centroids for a solution.
         */
        void recalculateCentroids();

        /**
         * @return Davies-Bouldin's S_i values for the solution.
         */
        float* calculateS();

        /**
         * @param S Davies-Bouldin's S_i values for the solution.
         *
         * @return Davies-Bouldin's R_{i,j} values for the solution.
         */
        float calculateRsum(float* S);

        /**
         * @return Distance intra-cluster.
         */
        float intra();

        /**
         * @return Distance inter-cluster.
         */
        float inter();

        /**
         * @return Turi validity index y(K).
         */
        float y();

        /**
         * Calculates the distance between two patterns in the database.
         *
         * @param i First pattern identificator.
         * @param j Second pattern identificator.
         *
         * @return Distance between the pattern i and j.
         */
        float d(int i, int j);

        /**
         * Calculates the distance between two patterns.
         *
         * @param a    First pattern.
         * @param b    Second pattern.
         * @param size Size of the patterns.
         *
         * @return Distance between patterns.
         */
        float d(float* a, float* b);

        /**
         * Calculates the euclidean distance between two patterns.
         *
         * @param a    First pattern.
         * @param b    Second pattern.
         * @param size Size of the patterns.
         *
         * @return Euclidean distance between patterns.
         */
        float euclideanDistance(float* a, float* b, int size);

        /**
         * Data set information (patterns, quantity and size).
         */
        Data* data;

        /**
         * Initial quantity of centroids.
         */
        int Kmax;

        /**
          * Random number generator.
          */
        MTStore drand;
};
#endif
