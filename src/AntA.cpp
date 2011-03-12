/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripci�n
 *
 * Clase concreta donde se definen las funciones y variables
 * necesarias para ejecutar la metaheur�stica K-means.
 */

#include "AntA.h"

/**
 * Constructor de la clase AntA.
 *
 * @param _d   Datos del problema.
 * @param _m   Dimensi�n de cada dato.
 * @param _n   Cantidad de datos.
 * @param _k   Cantidad de clusters (iniciales).
 * @param _nA  Cantidad de hormigas.
 * @param _it  Cantidad de iteraciones.
 * @param _met M�trica.
 */
AntA::AntA(float** _d, int _m, int _n, int _nA, int _it, int _met)
: Metaheuristic(_d, _m, _n, _n, _met){
   
    nA = _nA;
    maxit = _it;
    ac = false;

    free = new int[N];
    cells = new vector<int>[N];    
    ants = new Ant[nA];

    globalSize = 0;
    globalMemory = new int[GM];

}

/**
 * Destructor de la clase AntA.
 */
AntA::~AntA(){

    delete [] free;    
    delete [] cells;
    delete [] ants;   
    delete [] globalMemory; 

}

void AntA::run(int type){

    int i = 0, ra = 0;

    initialize();
    
    for(i = 0; i < maxit; i++){

        ra = rand()%nA;
        
        if(ants[ra].isFree())
            pickAnt(ra);
        else
            dropAnt(ra);
        
    }

    reconstruct();

    Kmeans *KA = new Kmeans(data, M, N, K, metric, 3);

    KA->setCentroids(bestCentroids);

    KA->run(type);

    for(i = 0; i < N; i++)
        bestSolution[i] = KA->bestSolution[i];

    bestFO = KA->bestFO;

    K = KA->K;

    delete KA;
    
}

void AntA::pickAnt(int ra){

    int rp = 0, rc = 0;
    float rn = 0.0;
    vector<int>::iterator it;

    rp = rand()%N;
    
    while(free[rp] == -1)
        rp = rand()%N;
        
    rc = free[rp];

    rn = ((float)rand())/((float)RAND_MAX);

    if(rn <= ppick(rp, rc)){

    
        for(it = cells[rc].begin(); it < cells[rc].end(); it++)
            if(*it == rp)
                cells[rc].erase(it);

        free[rp] = -1;
        ants[ra].pick(rp);
        
    }
}

void AntA::reconstruct(){

    float actual = 0.0, max = -1.0;
    int i = 0, j = 0, best = 0, rp = 0;
    int *count = new int[K];
    vector<int>::iterator it;
    
    for(i = 0; i < N; i++){
        if(free[i] == -1){
            max = -1.0;
            for(j = 0; j < N; j++){
                if(cells[j].size() >0){
                    if( (actual = f(i, j)) > max){
                        max = actual;
                        best = j;
                    }
                }
            }
            cells[best].push_back(i);
        }
    }

    for(i = 0; i < nA; i++){
        if(!ants[i].isFree()){
            max = -1.0;
            rp = ants[i].getPixel();
            for(j = 0; j < N; j++){
                if(cells[j].size() >0){
                    if((actual = f(rp, j)) > max){
                        max = actual;
                        best = j;
                    }
                }
            }
            cells[best].push_back(rp);
        }
    }

    K = 0;

    for(i = 0; i < N; i++){
        if(cells[i].size() > 0){

            for(it = cells[i].begin(); it < cells[i].end(); it++)
                bestSolution[*it] = K;

            K++;

        }
    }

    for(i = 0; i < K; i++){

        for(j = 0; j < M; j++)
            bestCentroids[i][j] = 0.0;

        count[i] = 0;

    }

    for(i = 0; i < N; i++){

        for(j = 0; j < M; j++)
            bestCentroids[bestSolution[i]][j] += data[i][j];

        count[bestSolution[i]]++;

    }

    for(i = 0; i < K; i++)
        for(j = 0; j < M; j++)
            bestCentroids[i][j] = bestCentroids[i][j] / count[i];
            
    delete [] count;

}

void AntA::dropAnt(int ra){

    int j = 0, best = 0, rc = ants[ra].getPixel(), rp = 0, actual = 0;
    float max = -1.0, rn = 0.0;
    bool done = false;
    
    if(globalSize < GM){
        if(ants[ra].getMSize() > 0){

            for( j = 0; j < ants[ra].getMSize(); j++){
                rc =ants[ra].getM(j);
                actual = f(rp, rc);
                if(actual > max){
                    max = actual;
                    best = rc;
                }
            }

            rc = best;
            rn = ((float)rand())/((float)RAND_MAX);

            if(rn <= pdrop(rp, rc)){
            
                cells[rc].push_back(rp);

                free[rp] = rc;
                ants[ra].drop(rc);
                done = true;

            }

            if(!done){

                rc = rand()%N;

                rn = ((float)rand())/((float)RAND_MAX);

                if(rn <= pdrop(rp, rc)){

                    cells[rc].push_back(rp);

                    free[rp] = rc;
                    ants[ra].drop(rc);

                }

            }

        }else{

            rc = rand()%N;

            rn = ((float)rand())/((float)RAND_MAX);

            if(rn <= pdrop(rp, rc)){

                cells[rc].push_back(rp);

                free[rp] = rc;
                ants[ra].drop(rc);

            }

        }
    }else{

        for( j = 0; j < globalSize; j++){
            rc = globalMemory[j];
            actual = f(rp, rc);
            if(actual > max){
                max = actual;
                best = rc;
            }
        }

        rc = best;
        rn = ((float)rand())/((float)RAND_MAX);

        if(rn <= pdrop(rp, rc)){
        
            cells[rc].push_back(rp);

            free[rp] = rc;
            ants[ra].drop(rc);
            done = true;

        }

        if(!done){
            
            if(ants[ra].getMSize() > 0){
            
                for( j = 0; j < ants[ra].getMSize(); j++){
                    rc =ants[ra].getM(j);
                    actual = f(rp, rc);
                    if(actual > max){
                        max = actual;
                        best = rc;
                    }
                }

                rc = best;
                rn = ((float)rand())/((float)RAND_MAX);

                if(rn <= pdrop(rp, rc)){
                
                    cells[rc].push_back(rp);

                    free[rp] = rc;
                    ants[ra].drop(rc);
                    done = true;

                }

                if(!done){

                    rc = rand()%N;

                    rn = ((float)rand())/((float)RAND_MAX);

                    if(rn <= pdrop(rp, rc)){

                        cells[rc].push_back(rp);

                        free[rp] = rc;
                        ants[ra].drop(rc);

                    }

                }

            }else{

                rc = rand()%N;

                rn = ((float)rand())/((float)RAND_MAX);

                if(rn <= pdrop(rp, rc)){

                    cells[rc].push_back(rp);

                    free[rp] = rc;
                    ants[ra].drop(rc);

                }

            }
            
        }
            

    }

    addMemory(rc);

}

void AntA::addMemory(int cell){

    bool done = false;
    int aux = 0, ant = 0, i = 0;

    for(i = 0; i < GM; i++){
        if(globalMemory[i] == cell){
            done = true;
            break;
        }
    }
    
    if(!done){

        if(globalSize < GM){
        
            globalMemory[globalSize] = cell;
            globalSize++;
            
        }else{
        
            aux = globalMemory[0];
            
            for(i = 1; i < globalSize; i++){
                ant = aux;
                aux = globalMemory[i];
                globalMemory[i] = ant;
            }
                
            globalMemory[0] = cell;
            
        }

    }

}

float AntA::ppick(int pixel, int cell){

    int size = cells[cell].size();

    if(size == 1)
        return 1.0;
    if(size == 2)
        return 0.7;
    else
        return pow(cos(PIH * f(pixel, cell)),2 );

}

float AntA::pdrop(int pixel, int cell){

    return 1 - pow(cos(PIH * f(pixel, cell)),2 );

}


float AntA::f(int pixel, int cell){

    float sum = 0.0;
    vector<int>::iterator it;
    int size = cells[cell].size();

    if(size == 0) return 0.0;
    
    for ( it= cells[cell].begin() ; it < cells[cell].end(); it++ ){
        sum += alpha2/(alpha2 + d(pixel, *it));
    }

        
    sum = sum / size;

    return sum;
    
}

/**
 * Intercambio la posici�n f y s, del arreglo dado.
 *
 * @param array Arreglo a intercambiar.
 * @param f Primero posici�n.
 * @param s Segundo posici�n.
 */
inline void AntA::swap(int *array, int f, int s){

    int aux = array[f];
    array[f] = array[s];
    array[s] = aux;

}


/**
 * Inicializa los centroides de los clusters.
 */
void AntA::initialize(){

    int i, k;
    int done[N];
    int size = N;

    for(i = 0; i < N; ++i){
        free[i] = i;
        done[i] = i;
        cells[i].push_back(i);
    }


    for(i = 0; i < nA ; ++i){
    
        k = done[rand()%size];
        swap(done, k, size);
        size--;
        
        ants[i].pick(k);
        free[k] = -1;

    }

    if(!ac){
        calcAlpha();
        ac = true;
    }

    

}

void AntA::calcAlpha(){

    int i, j;

    alpha2 = 0.0;
    
    for(i = 0; i < N; i++)
        for(j = i+1; j < N; j++)
            alpha2 += 2*d(i,j);

    alpha2 = pow(alpha2, 2);

}
