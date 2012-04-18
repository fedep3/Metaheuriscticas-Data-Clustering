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
#include "Individual.h"

#ifndef _GENETIC_INDIVIDUAL_
#define _GENETIC_INDIVIDUAL_

using namespace std;

class WrongClusterSizeException: public exception {
    public:
        WrongClusterSizeException() { }

        /**
         * Explanatory message of the exception.
         */
        virtual const char* what() const throw() {
            return "WrongClusterSizeException: Wrong cluster size. Can't perform this mutation. This should be already filtered.";
        }
};

class GeneticIndividual : public Individual {
    public:
        GeneticIndividual()
            : Individual() { };

        /**
         * @param data Data to be clustered.
         * @param K    Maximum quantity of clusters in the final solution.
         */
        GeneticIndividual(Data* data, int K)
            : Individual(data, K) { }

        /**
         * Copy constructor.
         */
        GeneticIndividual(const GeneticIndividual& individual)
            : Individual(individual) { }

        /**
         * Destructor.
         */
        virtual ~GeneticIndividual() { }

        /**
         * Mutates the individual.
         */
        void mutate();

        /**
         * Crosses this individual with another to generate two childs.
         *
         * @param parent      Other parent.
         * @param firstChild  First child.
         * @param secondChild Second child.
         */
        void crossover(GeneticIndividual& parent,
                       GeneticIndividual& firstChild,
                       GeneticIndividual& secondChild);

    protected:
        /**
         * Changes random centroid.
        */
        void changeRandomCentroid();

        /**
         * Adds a new cluster.
         */
        void addRandomCentroid();

        /**
         * Merges two clusters.
         */
        void mergeRandomCentroids();

        /**
         * Substitutes one parent with a child.
         *
         * @param parent Other parent.
         * @param child  Child.
         */
        void substituteByChild(GeneticIndividual& parent,
                               GeneticIndividual& child);

        /**
         * Generates two childs from this individual and another.
         *
         * @param parent      Other parent.
         * @param firstChild  First child.
         * @param secondChild Second child.
         */
        void generateChilds(GeneticIndividual& parent,
                            GeneticIndividual& firstChild,
                            GeneticIndividual& secondChild);
};
#endif
