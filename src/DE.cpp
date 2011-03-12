/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase concreta donde se definen las funciones y variables
 * necesarias para ejecutar la metaheurística K-means.
 */

#include "DE.h"

/**
 * Constructor de la clase DE.
 *
 * @param _d   Datos del problema.
 * @param _m   Dimensión de cada dato.
 * @param _n   Cantidad de datos.
 * @param _k   Cantidad de clusters (iniciales).
 * @param _i   Cantidad de individuos.
 * @param _t   Número de iteraciones.
 * @param _met Métrica.
 */
DE::DE(float** _d, int _m, int _n, int _k, int _i, int _it, float *_zmx,
                                                  float *_zmn, int _met)
: Metaheuristic(_d, _m, _n, _k, _met){

    int i, j;

    I = _i;

    Zmax = 0.0;
    for(i = 0; i < M; ++i)
        Zmax += (_zmx[i] - _zmn[i]) * (_zmx[i] - _zmn[i]);
    Zmax = sqrtf(Zmax);

    //Se reservan I vectores solución.
    solution    = (int **) calloc(I, sizeof(int*));
    if(solution == NULL) exit(1);
    for(i = 0; i < I; ++i)
        solution[i] = new int[N];

    //Se reservan I arreglos de centroides.
    centroid    = (float ***) calloc(I, sizeof(float**));
    if(centroid == NULL) exit(1);
    for(i = 0; i < I; ++i){
        centroid[i] = (float**) calloc(K, sizeof(float*));
        if(centroid[i] == NULL) exit(1);
        for(j = 0; j < K; ++j)
            centroid[i][j] = new float[M];
    }

    of = new float[I]; //Valor de las funciones objetivo.

    maxit = _it;
    var = true;
}

/**
 * Constructor de la clase DE.
 *
 * @param _d   Datos del problema.
 * @param _m   Dimensión de cada dato.
 * @param _n   Cantidad de datos.
 * @param _k   Cantidad de clusters (iniciales).
 * @param _i   Cantidad de individuos.
 * @param _it  Número de iteraciones.
 * @param _Cr  Probabilidad de cruce.
 * @param _F   Escalar por el cual se multiplica la diferencia, es un 
 *             parámetro.
 * @param _met Métrica.
 */
DE::DE(float** _d, int _m, int _n, int _k, int _i, int _it, float _Cr, 
                                   float _F, float *_zmx, float *_zmn, int _met)
: Metaheuristic(_d, _m, _n, _k, _met){
    int i, j;

    I = _i;

    Zmax = 0.0;
    for(i = 0; i < M; ++i)
        Zmax += (_zmx[i] - _zmn[i]) * (_zmx[i] - _zmn[i]);
    Zmax = sqrtf(Zmax);

    //Se reservan I vectores solución.
    solution    = (int **) calloc(I, sizeof(int*));
    if(solution == NULL) exit(1);
    for(i = 0; i < I; ++i)
        solution[i] = new int[N];

    //Se reservan I arreglos de centroides.
    centroid    = (float ***) calloc(I, sizeof(float**));
    if(centroid == NULL) exit(1);
    for(i = 0; i < I; ++i){
        centroid[i] = (float**) calloc(K, sizeof(float*));
        if(centroid[i] == NULL) exit(1);
        for(j = 0; j < K; ++j)
            centroid[i][j] = new float[M];
    }

    of = new float[I]; //Valor de las funciones objetivo.

    maxit = _it;
    F = _F;
    Cr = _Cr;
    var = false;
}

/**
 * Destructor de la clase DE.
 */
DE::~DE(){
    int i, j;

    for(i = 0; i < I; ++i)
        delete [] solution[i];
    free(solution);

    for(i = 0; i < I; ++i){
        for(j = 0; j < Kmax; ++j)
            delete [] centroid[i][j];
        free(centroid[i]);
    }
    free(centroid);
    
    delete [] of;
}


/**
 * Ejecuta la metaheurística con los datos dados.
 *
 * @param type Si se utilizará una función objetivo de
 *             maximización o de minimización.
 */
void DE::run(int type){

    float **auxCent;
    int *auxSol;
    int auxOf = 0;
    int m = 0, i = 0, j = 0, k = 0, l = 0, h = 0, p = 0, best = 0;

    
    auxCent = (float**) calloc(Kmax, sizeof(float*));
    if(auxCent == NULL) exit(1);
    for(j = 0; j < Kmax; ++j)
        auxCent[j] = new float[M];
        
    auxSol = new int[N];

    initialize();

    for(i = 0; i < I; i++){

        assign(solution[i], centroid[i]);

        of[i] = foMin(solution[i], centroid[i], K);

    }

    for(int it  = 0; it < maxit; it++){

        if(var){
            Cr = (CrMAX-CrMIN) * ((float) (maxit - it))/((float) maxit) + CrMIN;
            F = 0.5 * ( 1.0 + ((float)rand())/((float)RAND_MAX) );
        }

        for(i = 0; i < I; i++){

            m = rand() % I;

            while( m == i )
                m = rand() % I;

            k = rand() % I;

            while( k == m  || k == i)
                k = rand() % I;

            j = rand() % I;

            while( j == k || j == m || j == i)
                j = rand() % I;

            p = rand()%Kmax;

            for(l = 0; l < K; l++){
                if(((float)rand())/((float)RAND_MAX) < Cr || l == p){
                    for(h = 0; h < M; h++)
                        auxCent[l][h] = centroid[m][l][h] + F*(centroid[j][l][h] - centroid[k][l][h]);
                }else{
                    for(h = 0; h < M; h++)
                        auxCent[l][h] = centroid[i][l][h];
                }
            }

            assign(auxSol, auxCent);

            if((auxOf = foMin(auxSol, auxCent, K)) < of[i]){
            
                for(j = 0; j < N; j++)
                    solution[i][j] = auxSol[j];

                for(j = 0; j < K; j++)
                    for(m = 0; m < M; m++)
                        centroid[i][j][m] = auxCent[j][m];

                of[i] = auxOf;
            }  

        }

    }

    best = getBetter(type);

    for(j = 0; j < N; j++)
        bestSolution[j] = solution[best][j];

    for(j = 0; j < K; j++)
        for(m = 0; m < M; m++)
            bestCentroids[j][m] = centroid[best][j][m];

    bestFO = of[best];

    renamer(bestSolution, &K);

    for(i = 0; i < K; i++)
        delete [] auxCent[i];
    
    free(auxCent);
    
    delete [] auxSol;
    
}

/**
 * Función objetivo de la metaheurística PSO.
 *
 * @param sol  Vector solución.
 * @param cent Centroides asociados al vector solución.
 * @param k    Cantidad de clusters.
 * @param met  Tipo de función objetivo.
 *
 * @return Valor de la solución con función objetivo
 *         indicada.
 */
float DE::foMin(int* sol, float** cent, int k){
    return ( deFO(sol, cent, k) );
}


/**
 * Cuenta el número de clusters diferentes a los que pertenece un
 * datos.
 * @param solution Arreglo con la solución.
 * @return Cantidad de clusters diferentes.
 */
int DE::countClust(int *solution){
    
    int count[Kmax];
    int result = 0;
    int i;

    for(i = 0; i < Kmax; i++)
        count[i] = 0;
    
    for(i = 0 ; i < N ; i++)
        count[solution[i]]++;
        
    for(i = 0; i < Kmax; i++)
        if(count[i] > 0)
            result++;
            
    return result;
    
}


/**
 * Busca el mejor cluster para un objeto, tomando en cuanta los centroi-
 * des activados.
 *
 * @param centroid Centroides del individuo.
 * @param e Objeto.
 *
 * @return Mejor cluster para el objeto.
 */
int DE::bestCluster( float **centroid, int e){

    int j;
    float mn = numeric_limits<float>::infinity();
    float t;
    int c = 0;
    for(j = 0; j < Kmax; ++j){
        t = d(centroid[j], data[e]);
        if(mn > t){
            mn = t;
            c  = j;
        }
    }
    return c;
    
}

/**
 * Intercambio la posición f y s, del arreglo dado.
 *
 * @param array Arreglo a intercambiar.
 * @param f Primero posición.
 * @param s Segundo posición.
 */
void DE::swap(int *array, int f, int s){

    int aux = array[f];
    array[f] = array[s];
    array[s] = aux;

}


/**
 * Obtiene el mejor de la población.
 *
 * @param type Tipo de función objetivo.
 * @return Número de individuo mejor de la población.
 */
int DE::getBetter(int type){
    float best;
    int top = 0;
    int i;
    switch(type){
        case T_MAX:
            best = -1.0;
            break;
        default:
            best = numeric_limits<float>::infinity();
    }

    for(i = 0; i < I; ++i){
        switch(type){
            case T_MAX:
                if(of[i] > best){
                    best = of[i];
                    top = i;
                }
                break;
            default:
                if(of[i] < best){
                    best = of[i];
                    top = i;
                }
        }
    }

    return top;
}

/**
 * Inicializa los centroides de los clusters.
 */
void DE::initialize(){
    int i, j, l;
    int k, size = N;

    int done[N];

    for(i = 0; i < N; ++i)
        done[i] = i;

    for(i = 0; i < I ; ++i){
        for(j = 0; j < Kmax; ++j){
        
            k = done[rand()%size];
            swap(done, k, size);
            size--;

            for(l = 0; l < M; ++l)
                centroid[i][j][l] = data[k][l];
            
        }


    }

}

/**
 * Reasigna los elementos a cada cluster en una partícula.
 *
 * @param p Párticula.
 */
void DE::assign(int *solution, float **centroid){
    int i, j;
    float ma = 0.0;
    int mai  = 0;
    float t1  = 0.0;
    float t2  = 0.0;
    int largest;

    for(i = 0; i < Kmax; ++i)
        size[i] = 0;

    //Reasignar elementos a clusters.
    for(i = 0; i < N; ++i){
        solution[i] = bestCluster(centroid, i);

        size[ solution[i] ] += 1;

        t1 = d(data[i], centroid[solution[i]]);
        if(t1 > ma){
            ma  = t1;
            mai = i;
        } 
    }

    //Reinicializaciones
    largest = mai;
    ma = 0.0;
    mai = 0;

    //Asegurar que en la reasignación no queden clusters vacíos.
    for(i = 0; i < Kmax; ++i){
        switch(size[i]){
            case 0:
            case 1:
                //Creación de un nuevo cluster:
                solution[largest] = i;
                size[i] += 1;
                for(j = 0; j < M; ++j){
                    centroid[i][j] = data[largest][j];
                }

                //Llena el nuevo cluster
                for(j = 0; j < N; ++j){
                    t1 = d(data[j], centroid[i]);
                    t2 = d(data[j], centroid[solution[j]]);

                    if(t1 < t2){
                        size[solution[j]] -=1;
                        solution[j] = i;
                        size[i] += 1;
                    }else{
                        if(t2 > ma){
                            ma = t2;
                            mai = j;
                        }
                    }
                }
                //Reinicializaciones.
                i = 0;
                largest = mai;
                ma  = 0.0;
                mai = 0;
                break;
            default:
                break;
        }
    }

    return;
}

/**
 * Función objetivo del algoritmo PSO.
 *
 * @param sol  Vector solución.
 * @param cent Centroides asociados al vector solución.
 * @param k    Cantidad de clusters en el vector solución.
 *
 * @return Valor de la función objetivo.
 */
float DE::deFO(int* sol, float** cent, int k){
    int i, j;
    float dmax = 0.0;
    float je   = 0.0;
    float dmin = numeric_limits<float>::infinity();
    float t    = 0.0;

    float* sums = new float[k];
    for(i = 0; i < k; ++i){
        sums[i] = 0.0;
        size[i] = 0;
    }

    for(i = 0; i < N; ++i){
        sums[ sol[i] ] += d(data[i], cent[ sol[i] ]);
        size[ sol[i] ] += 1;
    }

    for(i = 0; i < k; ++i){
        if(size[i] != 0) 
            sums[i] = sums[i] / ( (float) size[i] );
    }

    //Cálculo de dmax.
    for(i = 0; i < k; ++i){
        je += sums[i];
        if(sums[i] > dmax)
            dmax = sums[i];
    }

    dmax = (0.5 * dmax) / Zmax;

    //Cálculo de dmin.
    for(i = 0; i < k; ++i){
        for(j = 0; j < k; ++j){
            if(i != j){
                t = d(cent[i], cent[j]);
                if(t < dmin)
                    dmin = t;
            }
        }
    }

    dmin = dmin / Zmax;

    //Cálculo de Je.
    je = je / k;
    je = (0.5 * je) / Zmax;

    delete [] sums;

    return ( (dmax + je) / dmin );

}
