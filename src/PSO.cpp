/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase concreta donde se definen las funciones y variables
 * necesarias para ejecutar el algoritmo de particle swarm
 * optimization (PSO).
 */
#include "PSO.h"

/**
 * Constructor de la clase PSO.
 *
 * @param _d   Datos del problema.
 * @param _m   Dimensión de cada dato.
 * @param _n   Cantidad de datos.
 * @param _k   Cantidad de clusters (iniciales).
 * @param _p   Cantidad de partículas.
 * @param _c1  Constante de la componente cognitiva
 *             de las partículas.
 * @param _c2  Constante de la componente social
 *             de las partículas.
 * @param _w   Peso inercial de las partículas.
 * @param _w1  Peso de la distancia de los elementos
 *             dentro de un cluster en la función
 *             objetivo.
 * @param _w2  Peso de la distancia entre clusters
 *             en la función objetivo.
 * @param _w3  Peso del error en la función objetivo.
 *             en la función objetivo.
 * @param _zmx Máximo valor de un atributo.
 * @param _zmn Mínimo valor de un atributo.
 * @param _vmx Velocidad máxima.
 * @param _fun Tipo de función objetivo.
 * @param _reps Repeticiones sin mejora.
 */
PSO::PSO(float** _d, int _m, int _n, int _k, int _p,
    float _c1, float _c2, int _w,
    float _w1, float _w2, float _w3,
    float* _zmx, float* _zmn, float _vmx,
    int _fun, int _reps)
: Metaheuristic(_d, _m, _n, _k, _fun){
    int i, j, l;
    P    = _p;
    c1   = _c1;
    c2   = _c2;
    W    = _w;
    w1   = _w1;
    w2   = _w2;
    w3   = _w3;
    REPS = _reps;

    Zmax = 0.0;
    for(i = 0; i < M; ++i)
        Zmax += (_zmx[i] - _zmn[i]) * (_zmx[i] - _zmn[i]);
    Zmax = sqrtf(Zmax);

    Vmax = _vmx;
    metric = F_WEIGHTED;

    //////////////////////////////////
    // Reservas para las partículas.

    //Se reservan P vectores solución.
    solution    = (int **) calloc(P, sizeof(int*));
    if(solution == NULL) exit(1);
    for(i = 0; i < P; ++i)
        solution[i] = new int[N];

    //Se reservan P "posiciones actuales".
    centroid    = (float ***) calloc(P, sizeof(float**));
    if(centroid == NULL) exit(1);

    //Se reservan P "vectores de velocidad".
    velocity    = (float ***) calloc(P, sizeof(float**));
    if(velocity == NULL) exit(1);

    //Se reservan P "mejores posiciones".
    bestParticle    = (float ***) calloc(P, sizeof(float**));
    if(bestParticle == NULL) exit(1);

    for(i = 0; i < P; ++i){
        centroid[i] = (float**) calloc(K, sizeof(float*));
        if(centroid[i] == NULL) exit(1);

        velocity[i] = (float**) calloc(K, sizeof(float*));
        if(velocity[i] == NULL) exit(1);

        bestParticle[i] = (float**) calloc(K, sizeof(float*));
        if(bestParticle[i] == NULL) exit(1);

        for(j = 0; j < K; ++j){
            centroid[i][j]     = new float[M];
            velocity[i][j]     = new float[M];
            for(l = 0; l < M; ++l) velocity[i][j][l] = 0.0;
            bestParticle[i][j] = new float[M];
        }
    }

    //Para mejores posiciones.
    of = new float[P]; //Valor de las funciones objetivo.
    Ks = new int[P];   //Cantidad de clusters.
    for(i = 0; i < P; ++i)
        Ks[i] = K;
}

/**
 * Constructor de la clase PSO.
 *
 * @param _d   Datos del problema.
 * @param _m   Dimensión de cada dato.
 * @param _n   Cantidad de datos.
 * @param _k   Cantidad de clusters (iniciales).
 * @param _p   Cantidad de partículas.
 * @param _c1  Constante de la componente cognitiva
 *             de las partículas.
 * @param _c2  Constante de la componente social
 *             de las partículas.
 * @param _w   Peso inercial de las partículas.
 * @param _zmx Máximo valor de un atributo.
 * @param _zmn Mínimo valor de un atributo.
 * @param _vmx Velocidad máxima.
 * @param _fun Tipo de función objetivo.
 * @param _reps Repeticiones sin mejora.
 */
PSO::PSO(float** _d, int _m, int _n, int _k, int _p,
    float _c1, float _c2, int _w,
    float* _zmx, float* _zmn, float _vmx,
    int _fun, int _reps)
: Metaheuristic(_d, _m, _n, _k, _fun){
    int i, j, l;
    P    = _p;
    c1   = _c1;
    c2   = _c2;
    W    = _w;
    w1   = 0.0;
    w2   = 0.0;
    w3   = 0.0;
    REPS = _reps;

    Zmax = 0.0;
    for(i = 0; i < M; ++i)
        Zmax += (_zmx[i] - _zmn[i]) * (_zmx[i] - _zmn[i]);
    Zmax = sqrtf(Zmax);

    Vmax = _vmx;
    metric = F_NON_WEIGHTED;

    //////////////////////////////////
    // Reservas para las partículas.

    //Se reservan P vectores solución.
    solution    = (int **) calloc(P, sizeof(int*));
    if(solution == NULL) exit(1);
    for(i = 0; i < P; ++i)
        solution[i] = new int[N];

    //Se reservan P "posiciones actuales".
    centroid    = (float ***) calloc(P, sizeof(float**));
    if(centroid == NULL) exit(1);

    //Se reservan P "vectores de velocidad".
    velocity    = (float ***) calloc(P, sizeof(float**));
    if(velocity == NULL) exit(1);

    //Se reservan P "mejores posiciones".
    bestParticle    = (float ***) calloc(P, sizeof(float**));
    if(bestParticle == NULL) exit(1);

    for(i = 0; i < P; ++i){
        centroid[i] = (float**) calloc(K, sizeof(float*));
        if(centroid[i] == NULL) exit(1);

        velocity[i] = (float**) calloc(K, sizeof(float*));
        if(velocity[i] == NULL) exit(1);

        bestParticle[i] = (float**) calloc(K, sizeof(float*));
        if(bestParticle[i] == NULL) exit(1);

        for(j = 0; j < K; ++j){
            centroid[i][j]     = new float[M];
            velocity[i][j]     = new float[M];
            for(l = 0; l < M; ++l) velocity[i][j][l] = 0.0;
            bestParticle[i][j] = new float[M];
        }
    }

    //Para mejores posiciones.
    of = new float[P]; //Valor de las funciones objetivo.
    Ks = new int[P];   //Cantidad de clusters.
    for(i = 0; i < P; ++i)
        Ks[i] = K;
}

/**
 * Destructor de la clase GA.
 */
PSO::~PSO(){
    int i, j;

    for(i = 0; i < P; ++i)
        delete [] solution[i];
    free(solution);

    for(i = 0; i < P; ++i){
        for(j = 0; j < Kmax; ++j){
            delete [] centroid[i][j];
            delete [] velocity[i][j];
            delete [] bestParticle[i][j];
        }
        free(centroid[i]);
        free(velocity[i]);
        free(bestParticle[i]);
    }
    free(centroid);
    free(velocity);
    free(bestParticle);

    delete [] of;
    delete [] Ks;
}

/**
 * Inicializa la población. (Soluciones, centroides y
 * tamaños de clusters.
 */
void PSO::initialize(){
    int i, j, l;
    int k;

    bool* done = new bool[N];
    for(i = 0; i < N; ++i)
        done[i] = false;

    for(i = 0; i < P ; ++i){
        for(j = 0; j < K; ++j){
            k = rand() % N;
            
            while(done[k])
                k = rand() % N;
                
            done[k] = true;

            for(l = 0; l < M; ++l)
                centroid[i][j][l] = data[k][l];

        }

        assign(i);
    }

    bestFO = numeric_limits<float>::infinity();

    delete [] done;
}

/**
 * Ejecuta la metaheurística con los datos dados.
 *
 * @param type No aplica para éste caso.
 */
void PSO::run(int type){
    int p;
    int count = 0;
    float fo = 0.0;
    bool improved = false;

    initialize();

    while(count < REPS){
        for(p = 0; p < P; ++p){
            //Reasignar partícula.
            assign(p);
            fo = foMin(p, Kmax, 0);

            //Actualiza los mejores.
            updateBestParticle(p, fo);

            if(!improved)
                improved = updateBest(p, fo);
            else
                updateBest(p, fo);

            //Actualiza la velocidad de la partícula.
            updateVelocity(p);

            //Actualiza posición de la partícula.
            updateParticle(p);
        }

        if(improved){
            count = 0;
            improved = false;
        }else
            ++count;
    }

    renamer(bestSolution, &K);
}

/**
 * Actualiza la velocidad de la partícula.
 *
 * @param p Partícula.
 */
void PSO::updateVelocity(int p){
    int i, j;
    float r1, r2, v;

    for(i = 0; i < Kmax; ++i){
        for(j = 0; j < M; ++j){
            r1 = ( (float)rand() ) / ( (float)RAND_MAX );
            r2 = ( (float)rand() ) / ( (float)RAND_MAX );
            v = W * velocity[p][i][j] +
                c1 * r1 * (bestParticle[p][i][j] - centroid[p][i][j]) +
                c2 * r2 * (bestCentroids[i][j]   - centroid[p][i][j]);

            if(v < Vmax){
                velocity[p][i][j] = v;
            }
        }
    }

    return;
}

/**
 * Actualiza una partícula de acuerdo a su velocidad.
 *
 * @param p Partícula.
 */
void PSO::updateParticle(int p){
    int i, j;

    for(i = 0; i < Kmax; ++i)
        for(j = 0; j < M; ++j)
            centroid[p][i][j] += velocity[p][i][j];

    return;
}

/**
 * Actualiza la mejor posición de la partícula.
 *
 * @param p  Partícula.
 * @param fo Valor de la función objetivo.
 */
void PSO::updateBestParticle(int p, float fo){
    int i = 0, j = 0;

    if(of[p] > fo){
        of[p] = fo;

        for(i = 0; i < Kmax; ++i)
            for(j = 0; j < M; ++j)
                bestParticle[p][i][j] = centroid[p][i][j];
    }
}

/**
 * Actualiza el mejor global de la población.
 *
 * @param p  Partícula.
 * @param fo Valor de la función objetivo.
 *
 * @return Si hubo un cambio en la mejor solución global.
 */
bool PSO::updateBest(int p, float fo){
    int i = 0, j = 0;
    if(bestFO > fo){
        bestFO = fo;

        //Copia de los centroides.
        for(i = 0; i < Kmax; ++i)
            for(j = 0; j < M; ++j)
                bestCentroids[i][j] = centroid[p][i][j];

        //Copia del vector solución.
        for(i = 0; i < N; ++i)
            bestSolution[i] = solution[p][i];

        return true;
    }

    return false;
}

/**
 * Reasigna los elementos a cada cluster en una partícula.
 *
 * @param p Párticula.
 */
void PSO::assign(int p){
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
        solution[p][i] = bestCluster(p, i);

        size[ solution[p][i] ] += 1;

        t1 = d(data[i], centroid[p][solution[p][i]]);
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
                solution[p][largest] = i;
                size[i] += 1;
                for(j = 0; j < M; ++j){
                    centroid[p][i][j] = data[largest][j];
                }

                //Llena el nuevo cluster
                for(j = 0; j < N; ++j){
                    t1 = d(data[j], centroid[p][i]);
                    t2 = d(data[j], centroid[p][solution[p][j]]);

                    if(t1 < t2){
                        size[solution[p][j]] -=1;
                        solution[p][j] = i;
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
 * Función objetivo de la metaheurística PSO.
 *
 * @param i   Individuo.
 * @param k   Cantidad de clusters.
 * @param met Tipo de función objetivo.
 *
 * @return Valor de la solución con función objetivo
 *         indicada.
 */
float PSO::foMin(int i, int k, int fun){
    return ( psoFO(solution[i], centroid[i], k) );
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
float PSO::foMin(int* sol, float** cent, int k, int fun){
    return ( psoFO(sol, cent, k) );
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
float PSO::psoFO(int* sol, float** cent, int k){
    int i, j;
    float dmax = 0.0;
    float je   = 0.0;
    float dmin = numeric_limits<float>::infinity();
    float t    = 0.0;
    float res  = 0.0;

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

    switch(metric){
        case F_NON_WEIGHTED:
            dmax = (0.5 * dmax) / Zmax;
            break;
        default:
            res += w1 * dmax;
    }

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

    switch(metric){
        case F_NON_WEIGHTED:
            dmin = dmin / Zmax;
            break;
        default:
            res += w2 * ( Zmax - dmin );
    }

    //Cálculo de Je.
    je = je / k;

    switch(metric){
        case F_NON_WEIGHTED:
            je = (0.5 * je) / Zmax;
            break;
        default:
            res += w3 * je;
    }

    delete [] sums;

    switch(metric){
        case F_NON_WEIGHTED:
            return ( (dmax + je) / dmin );
            break;
        default:
            return res;
    }
}
