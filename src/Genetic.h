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
 * Concrete class where it's defined the Genetic algorithm for numerical data
 * clustering.
 */
#include <cstdio>
#include <time.h>
#include "RandomArray.h"
#include "Heuristic.h"
#include "GeneticIndividual.h"

#ifndef _GENETIC_
#define _GENETIC_

using namespace std;

class Genetic : public Heuristic<GeneticIndividual>{
    public:
        /**
         * @param data             Data to be clustered.
         * @param K                Maximum quantity of clusters in the final solution.
         * @param I                Number of individuals.
         * @param tournamentSize   Tournament size.
         * @param crossoverRate    Crossover rate.
         * @param mutationRate     Mutation rate.
         * @param improvementLevel Improvement level of the solution.
         */
        Genetic(Data* data, int K, int I,
                int tournamentSize, float crossoverRate, float mutationRate,
                int improvementLevel);

        /**
         * Destructor.
         */
        ~Genetic();

        /**
         * Executes the Genetic algorithm.
         */
        virtual void run();

    protected:
        /**
         * @return Whether there is crossover of two individuals or not.
         */
        bool isThereCrossover();

        /**
         * @return Whether there is mutation of one of the individuals or not.
         */
        bool isThereMutation();

        /**
         * @return Chosen individual identificator.
         */
        int chooseIndividualByTournament();

        /**
         * Crosses over tho individuals of the population.
         */
        void crossover();

        /**
         * Mutates one individual of the population.
         */
        void mutation();

        /**
         * @return Current generation's best individual identificator. 
         */
        int getGenerationBestIndividualID();

        /**
         * Tournament size.
         */
        int tournamentSize;

        /**
         * Crossover rate.
         */
        float crossoverRate;
        /**
         * Mutation rate.
         */
        float mutationRate;

        /**
         * Improvement level of the algorithm.
         */
        int improvementLevel;

        /**
         * First child of every generarion.
         */
        GeneticIndividual firstChild;

        /**
         * Second child of every generation.
         */
        GeneticIndividual secondChild;
};
#endif
