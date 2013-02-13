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
 * Concrete class to read CSV files for a data clustering algorithm and
 * write its results in a file.
 */
#include "Csv.h"

/**
 * Constructor.
 */
Csv::Csv(){ }

/**
 * Destructor.
 */
Csv::~Csv(){ }

/**
 * Reads the file.
 *
 * @param inputFile Input file.
 */
void Csv::read(const char* inputFile){
    int I = 0;
    int J = 0;
    int i, j, k;
    bool first = true;
    ifstream lines(inputFile);
    vector<float> v;

    string line;
    while(getline(lines,line)){
        if(strcmp(line.c_str(), "") == 0) continue;

        char * pch;
        pch = strtok (((char *)line.c_str()),",");

        while (pch != NULL){
            if(first) ++J;
            v.push_back(atof(pch));
            pch = strtok (NULL, ",");
            ++j;
        }

        if(!first){
            if(J != j){
                fprintf(stderr, "Error en the line %d of %s.\n", (I + 1), inputFile);
                fprintf(stderr, "Quantity of attributes does not coincide.\n");
                exit(1);
            }
        }

        j = 0;
        first = false;
        ++I;
    }

    data.N = I;
    data.M = J;

    lines.close();

    data.pattern = new float*[data.N];
    k = 0;
    for(i = 0; i < data.N; ++i){
        data.pattern[i] = new float[data.M];
        for(j = 0; j < data.M; ++j){
            data.pattern[i][j] = v[k];
            ++k;
        }
    }
}

/**
 * Writes the results in an output file.
 *
 * @param outputFile Output file.
 * @param solution   Solution to be written.
 * @param centroid   Centroids for the solution.
 * @param K          Quantity of clusters for the solution.
 */
void Csv::write(const char* outputFile, int* solution, float** centroid, int K){
    int i, j, l;
    ofstream lines(outputFile);

    for(l = 0; l < K; ++l){
        lines << "Cluster " << l << endl;

        j = 0;
        lines << "Centroid " << ":\t" << centroid[l][j];
        for(j = 0; j < data.M; ++j)
            lines << ", " << centroid[l][j];
        lines << endl;

        for(i = 0; i < data.N; ++i) {
            if(solution[i] == l) {
                j = 0;
                lines << "Object " << i << ":\t" << data.pattern[i][j]; 
                for(j = 1; j < data.M; ++j)
                    lines << ", " << data.pattern[i][j];
                lines << endl;
            }
        }
        lines << endl;
    }

    lines.close();
}
