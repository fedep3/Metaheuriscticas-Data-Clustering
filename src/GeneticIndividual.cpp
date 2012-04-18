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
 * Definition of an individual (solution) for the genetic algorithm.
 */
#include "GeneticIndividual.h"

#define CHANGE_CENTROID 0
#define ADD_CENTROID    1
#define MERGE_CENTROIDS 2

#define SWAP(x,y) { int t; t=x; x=y; y=t; }

/**
 * Mutates the individual.
 */
void GeneticIndividual::mutate() {
    int randomMutation = randomInteger(&drand, 0, 3);

    switch(randomMutation) {
        case CHANGE_CENTROID:
            changeRandomCentroid();
            break;
        case ADD_CENTROID:
            addRandomCentroid();
            break;
        case MERGE_CENTROIDS:
            mergeRandomCentroids();
            break;
        default:
            ;
    }
}

/**
 * Changes random centroid.
*/
void GeneticIndividual::changeRandomCentroid() {
    int i, patternID;
    int centroidID = randomInteger(&drand, 0, K);

    for(i = 0; i < data->M; ++i) {
        patternID = randomInteger(&drand, 0, data->N);
        centroid[centroidID][i] = data->pattern[patternID][i];
    }

    rearrange();
}

/**
 * Adds a new cluster.
 */
void GeneticIndividual::addRandomCentroid() {
    int i, patternID;

    try {
        if(K + 1 > Kmax)
            throw WrongClusterSizeException();

        patternID = randomInteger(&drand, 0, data->N);
        for(i = 0; i < data->M; ++i)
            centroid[K][i] = data->pattern[patternID][i];

        ++K;

        rearrange();
    } catch(const WrongClusterSizeException& e) {
        changeRandomCentroid();
    }
}

/**
 * Merges two clusters.
 */
void GeneticIndividual::mergeRandomCentroids() {
    int i, firstCentroidID, secondCentroidID;

    try {
        if (K <= 1)
            throw WrongClusterSizeException();

        RandomArray rarr(K);
        firstCentroidID  = rarr.get();
        secondCentroidID = rarr.get();
        
        if(firstCentroidID > secondCentroidID)
            SWAP(firstCentroidID, secondCentroidID);
        
        for(i = 0; i < data->M; ++i)
            centroid[firstCentroidID][i] = (centroid[firstCentroidID][i] + centroid[secondCentroidID][i])/2.0;

        rearrange();
    } catch(const WrongClusterSizeException& e) {
        addRandomCentroid();
    }
}

/**
 * Crosses this individual with another to generate two childs.
 *
 * @param parent      Other parent.
 * @param firstChild  First child.
 * @param secondChild Second child.
 */
void GeneticIndividual::crossover(GeneticIndividual& parent,
                                  GeneticIndividual& firstChild,
                                  GeneticIndividual& secondChild) {
    (*this).generateChilds(parent, firstChild, secondChild);

    (*this).substituteByChild(parent, firstChild);
    (*this).substituteByChild(parent, secondChild);
}

/**
 * Substitutes one parent with a child.
 *
 * @param parent Other parent.
 * @param child  Child.
 */
void GeneticIndividual::substituteByChild(GeneticIndividual& parent,
                                          GeneticIndividual& child) {
    if(parent.isBetterThan(*this)) {
        if(child.isBetterThan(*this))
            (*this).swap(child);
        else if(child.isBetterThan(parent))
            parent.swap(child);
    } else {
        if(child.isBetterThan(parent))
            parent.swap(child);
        else if(child.isBetterThan(*this))
            (*this).swap(child);
    }
}

/**
 * Generates two childs from this individual and another.
 *
 * @param parent      Other parent.
 * @param firstChild  First child.
 * @param secondChild Second child.
 */
void GeneticIndividual::generateChilds(GeneticIndividual& parent,
                                       GeneticIndividual& firstChild,
                                       GeneticIndividual& secondChild) {
    int i, j, point;
    if(K < parent.K)
        point = randomInteger(&drand, 0, K);
    else
        point = randomInteger(&drand, 0, parent.K);

    i = 0;
    while(i < K || i < parent.K) {
        for(j = 0; j < data->M; ++j) {
            if(i < point) {
                firstChild.centroid[i][j]  = centroid[i][j];
                secondChild.centroid[i][j] = parent.centroid[i][j];
            } else {
                if(i < (*this).K)
                    secondChild.centroid[i][j] = centroid[i][j];

                if(i < parent.K)
                    firstChild.centroid[i][j]  = parent.centroid[i][j];
            }
        }
        ++i;
    }

    firstChild.K  = parent.K;
    secondChild.K = K;

    firstChild.rearrange();
    secondChild.rearrange();
}
