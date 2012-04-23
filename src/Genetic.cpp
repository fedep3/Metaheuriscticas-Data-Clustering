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
#include "Genetic.h"

/**
 * @param data             Data to be clustered.
 * @param K                Maximum quantity of clusters in the final solution.
 * @param I                Number of individuals.
 * @param tournamentSize   Tournament size.
 * @param crossoverRate    Crossover rate.
 * @param mutationRate     Mutation rate.
 * @param improvementLevel Improvement level of the solution.
 */
Genetic::Genetic(Data* data, int K, int I,
                 int tournamentSize, float crossoverRate, float mutationRate,
                 int improvementLevel)
    : Heuristic<GeneticIndividual>(data, K, I) {
    this->tournamentSize   = tournamentSize;
    this->crossoverRate    = crossoverRate;
    this->mutationRate     = mutationRate;
    this->improvementLevel = improvementLevel;

    firstChild.initialize(data, K);
    secondChild.initialize(data, K);
}

/**
 * Destructor.
 */
Genetic::~Genetic() { }

/**
 * Executes the Genetic algorithm.
 */
void Genetic::run(){
    int count = 0;
    int generationBestIndividualID;

    while(count < improvementLevel) {
        if(isThereCrossover())
            crossover();

        if(isThereMutation())
            mutation();

        generationBestIndividualID = getGenerationBestIndividualID();

        if( individual[generationBestIndividualID].isBetterThan(bestIndividual) ) {
            bestIndividual = individual[generationBestIndividualID];
            count = 0;
        } else
            ++count;
    }
}

/**
 * @return Whether there is crossover of two individuals or not.
 */
bool Genetic::isThereCrossover() {
    return (mtGetRandomFloat(&drand) <= crossoverRate);
}

/**
 * @return Whether there is mutation of one of the individuals or not.
 */
bool Genetic::isThereMutation() {
    return (mtGetRandomFloat(&drand) <= mutationRate);
}

/**
 * @return Chosen individual identificator.
 */
int Genetic::chooseIndividualByTournament(int bannedID) {
    int i, currentIndividualID;

    RandomArray rarr(I);

    int bestIndividualID = rarr.get();
    if(bestIndividualID == bannedID)
        bestIndividualID = rarr.get();

    for(i = 0; i < (tournamentSize - 1); ++i) {
        currentIndividualID = rarr.get();
        if(currentIndividualID == bannedID)
            currentIndividualID = rarr.get();

        if(currentIndividualID == -1)
            break;

        if( individual[currentIndividualID].isBetterThan(individual[bestIndividualID]) )
            bestIndividualID = currentIndividualID;
    }

    return bestIndividualID;
}

/**
 * Crosses over tho individuals of the population.
 */
void Genetic::crossover() {
    int firstParentID  = chooseIndividualByTournament();
    int secondParentID = chooseIndividualByTournament(firstParentID);

    individual[firstParentID].crossover(individual[secondParentID], firstChild, secondChild);
}

/**
 * Mutates one individual of the population.
 */
void Genetic::mutation() {
    int individualID = randomInteger(&drand, 0, I);

    individual[individualID].mutate();
}

/**
 * @return Current generation's best individual identificator. 
 */
int Genetic::getGenerationBestIndividualID() {
    int i;
    int bestIndividualID = 0;

    for(i = 1; i < I; ++i) {
        if( individual[i].isBetterThan(individual[bestIndividualID]) )
            bestIndividualID = i;
    }

    return bestIndividualID;
}
