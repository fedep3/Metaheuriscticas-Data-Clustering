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
#include "Individual.h"

Individual::Individual() {
    data = NULL;
    K    = 0;

    Kmax = 0;
    evaluationOfDB = 0;
    indexValue = numeric_limits<float>::infinity();

    centroid = NULL;
    solution = NULL;

    drand.mtRandomInit(&drand, time(NULL), K_2M31);
}

/**
 * @param data Data to be clustered.
 * @param K    Maximum quantity of clusters in the final solution.
 */
Individual::Individual(Data* data, int K) {
    int i, j, p;

    this->data = data;
    this->K    = K;

    Kmax = K;
    evaluationOfDB = 0;
    indexValue = numeric_limits<float>::infinity();

    RandomArray rarr((*data).N);

    centroid = new float*[K];
    for(i = 0; i < K; ++i) {
        p = rarr.get();

        centroid[i] = new float[(*data).M];
        for(j = 0; j < (*data).M; ++j)
            centroid[i][j] = (*data).pattern[p][j];
    }

    solution = new int[(*data).N];

    drand.mtRandomInit(&drand, time(NULL), K_2M31);

    rearrange();
}

/**
 * @param data Data to be clustered.
 * @param K    Maximum quantity of clusters in the final solution.
 */
void Individual::initialize(Data* data, int K) {
    int i, j, p;

    this->data = data;
    this->K    = K;

    Kmax = K;
    evaluationOfDB = 0;
    indexValue = numeric_limits<float>::infinity();

    RandomArray rarr((*data).N);

    centroid = new float*[K];
    for(i = 0; i < K; ++i) {
        p = rarr.get();

        centroid[i] = new float[(*data).M];
        for(j = 0; j < (*data).M; ++j)
            centroid[i][j] = (*data).pattern[p][j];
    }

    solution = new int[(*data).N];

    drand.mtRandomInit(&drand, time(NULL), K_2M31);

    rearrange();

    
}

/**
 * Destructor.
 */
Individual::~Individual() {
    int i;

    for(i = 0; i < Kmax; ++i)
        delete [] centroid[i];
    delete [] centroid;

    delete [] solution;
}

/**
 * Copy constructor.
 */
Individual::Individual(const Individual& individual) {
    int i, j;

    data = individual.data;
    K    = individual.K;

    Kmax = individual.Kmax;
    evaluationOfDB = individual.evaluationOfDB;
    indexValue = individual.indexValue;

    centroid = new float*[Kmax];
    for(i = 0; i < K; ++i) {
        centroid[i] = new float[data->M];
        for(j = 0; j < data->M; ++j)
            centroid[i][j] = individual.centroid[i][j];
    }

    solution = new int[data->N];
    for(i = 0; i < data->N; ++i)
        solution[i] = individual.solution[i];

    drand.mtRandomInit(&drand, time(NULL), K_2M31);
}

/**
 * Assign operator.
 */
Individual & Individual::operator=(const Individual & individual) {
    int i, j;

    if(this != &individual) {
        data = individual.data;
        K    = individual.K;

        Kmax = individual.Kmax;
        evaluationOfDB = individual.evaluationOfDB;
        indexValue     = individual.indexValue;

        bool initialized = true;
        if(centroid == NULL)
            initialized = false;

        if(!initialized)
            centroid = new float*[Kmax];

        for(i = 0; i < Kmax; ++i) {
            if(!initialized)
                centroid[i] = new float[data->M];

            for(j = 0; i < K && j < data->M; ++j)
                centroid[i][j] = individual.centroid[i][j];
        }

        if(!initialized)
            solution = new int[data->N];

        for(i = 0; i < data->N; ++i)
            solution[i] = individual.solution[i];

        drand.mtRandomInit(&drand, time(NULL), K_2M31);
    }

    return *this;
}

/**
 * Swaps individuals.
 *
 * @param individual Individual to be swaped.
 */
void Individual::swap(Individual & individual) {
    int ti;
    float tf;
    float** tc;
    int* ts;
    if(this != &individual) {
        ti           = K;
        K            = individual.K;
        individual.K = ti;

        ti              = Kmax;
        Kmax            = individual.Kmax;
        individual.Kmax = ti;

        tf                    = indexValue;
        indexValue            = individual.indexValue;
        individual.indexValue = tf;

        tc                  = centroid;
        centroid            = individual.centroid;
        individual.centroid = tc;

        ts                  = solution;
        solution            = individual.solution;
        individual.solution = ts;

        evaluationOfDB += individual.evaluationOfDB;
        individual.evaluationOfDB = 0;
    }
}

/**
 * @return Whether the individual is better than other or not.
 */
bool Individual::isBetterThan(Individual & individual) {
    return (indexValue < individual.indexValue);
}

/**
 * Rearrange the solution.
 */
void Individual::rearrange() {
    reassign();
    relabel();
    recalculateCentroids();
    indexValue = DB();
}

/**
 * Reassign the patterns to the best cluster for them.
 */
void Individual::reassign() {
    int i;

    for(i = 0; i < data->N; ++i)
        solution[i] = getBestClusterIndex(i);
}

/**
 * @param patternID Pattern identificator.
 *
 * @return Index for the best cluster for the pattern.
 */
int Individual::getBestClusterIndex(int patternID) {
    float minimumDistance = numeric_limits<float>::infinity();
    int bestClusterIndex  = -1;

    float distance;
    int i;
    for(i = 0; i < K; ++i) {
        distance = d((*data).pattern[patternID], centroid[i]);
        if(minimumDistance > distance){
            minimumDistance  = distance;
            bestClusterIndex = i;
        }
    }

    return bestClusterIndex;
}

/**
 * Calculates the distance between two patterns in the database.
 *
 * @param i First pattern identificator.
 * @param j Second pattern identificator.
 *
 * @return Distance between the pattern i and j.
 */
float Individual::d(int i, int j){
    if(i == j)
        return 0.0;
    else
        return euclideanDistance(data->pattern[i], data->pattern[j], data->M);
}

/**
 * Calculates the distance between two patterns.
 *
 * @param a    First pattern.
 * @param b    Second pattern.
 * @param size Size of the patterns.
 *
 * @return Distance between patterns.
 */
float Individual::d(float* a, float* b){
    return euclideanDistance(a, b, data->M);
}

/**
 * Calculates the euclidean distance between two patterns.
 *
 * @param a    First pattern.
 * @param b    Second pattern.
 * @param size Size of the patterns.
 *
 * @return Euclidean distance between patterns.
 */
float Individual::euclideanDistance(float* a, float* b, int size){
    int i;
    float sum = 0.0, temp;

    for(i = 0; i < size; ++i) {
        temp = a[i] - b[i];
        sum += temp * temp;
    }

    return sqrt(sum);
}

/**
 * Re-labels the clusters identificators in the solution.
 */
void Individual::relabel() {
    map<int, int> label;
    int lastLabel = 0, currentLabel, i;

    for(i = 0; i < data->N; ++i) {
        currentLabel = solution[i];

        if( label.count( currentLabel ) == 0) {
            label[currentLabel] = lastLabel;
            ++lastLabel;
        }

        solution[i] = label[currentLabel];
    }

    K = lastLabel;
}

/**
 * Recalculate centroids for a solution.
 */
void Individual::recalculateCentroids() {
    int i, j, currentCluster;

    int* clusterSize = new int[K];
    for(i = 0; i < K; ++i) {
        for(j = 0; j < data->M; ++j)
            centroid[i][j] = 0.0;
        clusterSize[i] = 0;
    }

    for(i = 0; i < data->N; ++i) {
        currentCluster = solution[i];
        for(j = 0; j < data->M; ++j)
            centroid[currentCluster][j] += (*data).pattern[i][j];
        clusterSize[currentCluster] += 1;
    }

    for(i = 0; i < K; ++i)
        for(j = 0; j < data->M; ++j)
            centroid[i][j] /= clusterSize[i];

    delete [] clusterSize;
}

/**
 * @return Davies-Bouldin index value for the solution.
 */
float Individual::DB() {
    float Rsum = 0.0, *S = NULL;

    ++evaluationOfDB;

    try {
        if(K <= 1)
            throw ClusterSizeTooLowException();

        S    = calculateS();
        Rsum = calculateRsum(S);

        delete [] S;

        return (Rsum / K);
    } catch(const ClusterSizeTooLowException& e) {
        if(S != NULL)
            delete [] S;

        return numeric_limits<float>::infinity(); 
    }
}

/**
 * @return Davies-Bouldin's S_i values for the solution.
 */
float* Individual::calculateS() {
    int i, cluster;
    float* S         = new float[K];
    int* clusterSize = new int[K];

    for(i = 0; i < K; ++i){
        S[i]           = 0.0;
        clusterSize[i] = 0;
    }

    for(i = 0; i < data->N; ++i){
        cluster               = solution[i];
        S[cluster]           += d((*data).pattern[i], centroid[cluster]);
        clusterSize[cluster] += 1;
    }

    for(i = 0; i < K; ++i){
        if(clusterSize[i] <= 1) {
                delete [] S;
                delete [] clusterSize;
                throw ClusterSizeTooLowException();
        }

        S[i] = S[i] / ( (float) clusterSize[i] );
    }

    delete [] clusterSize;

    return S;
}

/**
 * @param S Davies-Bouldin's S_i values for the solution.
 *
 * @return Davies-Bouldin's R_{i,j} values for the solution.
 */
float Individual::calculateRsum(float* S) {
    int i, j;
    float maximum, distance, R;
    float sum = 0.0;
    

    for(i = 0; i < K; ++i) {
        maximum = -1.0;
        for(j = 0; j < K; ++j){
            if(i == j) continue;

            distance = d(centroid[i], centroid[j]);
            if(distance == 0.0)
                throw ClusterSizeTooLowException();

            R = ( (S[i] + S[j]) / distance );
            maximum = (maximum < R) ? R : maximum;
        }

        sum += maximum;
    }

    return sum;
}

/**
 * @return Turi validity index for the solution.
 */
float Individual::Turi() {
    return y() * ( intra() / inter() );
}

/**
 * @return Distance intra-cluster.
 */
float Individual::intra(){
    float sum   = 0.0;

    for(int i = 0; i < data->N; ++i)
        sum += d((*data).pattern[i], centroid[ solution[i] ]);

    return (sum / data->N);
}

/**
 * @return Distance inter-cluster.
 */
float Individual::inter(){
    int i, j;
    float minimum = numeric_limits<float>::infinity();
    float distance;

    for(i = 0; i < K; ++i) {
        for(j = i + 1; j < K; ++j) {
            if(i == j) continue;

            distance = d(centroid[i], centroid[j]);
            if(minimum > distance)
                minimum = distance;
        }
    }

    return minimum;
}

/**
 * @return Turi validity index y(K).
 */
float Individual::y(){
    int numerator  = (K - TURI_MEAN);
    float variance = (float) (TURI_STD_DEVIATION * TURI_STD_DEVIATION);
    float exponent = ((float) ( numerator * numerator) )/ (2.0 * variance);
    float n = (1.0 / sqrtf(2.0 * M_PI * variance)) * expf( -exponent );

    return TURI_C * n + 1.0;
}
