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
 * Definition of the abstract class BaseHeuristic. All the algorithms inherit
 * the base methods defined here.
 */
#include <cstdio>
#include <cstdlib>
#include "Individual.h"

#ifndef _BASE_HEURISTIC_
#define _BASE_HEURISTIC_

using namespace std;

class BaseHeuristic {
    public:
        BaseHeuristic() { }

        /**
         * Destructor.
         */
        virtual ~BaseHeuristic() { }

        /**
         * Executes the heuristic.
         */
        virtual void run() = 0;

        /**
         * @return Best solution Davies-Bouldin validity index value.
         */
        virtual float finalDB() = 0;

        /**
         * @return Best solution Turi validity index value.
         */
        virtual float finalTuri() = 0;

        /**
         * @return Number of evaluations of the objective function.
         */
        virtual int getNumberOfEvaluations() = 0;

        /**
         * Best individual found by the algorithm.
         */
        Individual bestIndividual;
};
#endif
