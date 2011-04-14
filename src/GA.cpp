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

#include "GA.h"

/**
 * Constructor de la clase Kmeans.
 *
 * @param _d   Datos del problema.
 * @param _m   Dimensión de cada dato.
 * @param _n   Cantidad de datos.
 * @param _k   Cantidad de clusters (iniciales).
 * @param _i   Cantidad de individuos.
 * @param _cp  Porcentaje de cruce.
 * @param _mut Porcentaje de mutación.
 * @param _met Métrica.
 * @param _reps Repeticiones sin mejora.
 */
GA::GA(float** _d, int _m, int _n, int _k, int _i,
       float _cp, float _mut, int _ts,
       int _met, int _reps)
: Metaheuristic(_d, _m, _n, _k, _met){
    int i, j;
    I      = _i;
    CP     = _cp;
    MUT    = _mut;
    REPS   = _reps;

    ////////////////////////////////
    // Reservas para la población.

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
    Ks = new int[I];   //Cantidad de clusters.
    for(i = 0; i < I; ++i)
        Ks[i] = K;

    torneoSize = _ts;

    ////////////////////////////////
    // Reservas para los hijos.

    s1 = new int[N];
    centroidS1 = (float**) calloc(K, sizeof(float*));
    if(centroidS1 == NULL) exit(1);
    for(j = 0; j < K; ++j)
        centroidS1[j] = new float[M];

    s2 = new int[N];
    centroidS2 = (float**) calloc(K, sizeof(float*));
    if(centroidS2 == NULL) exit(1);
    for(j = 0; j < K; ++j)
        centroidS2[j] = new float[M];
}

/**
 * Destructor de la clase Kmeans.
 */
GA::~GA(){
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

    delete [] s1;
    delete [] s2;
    for(i = 0; i < Kmax; ++i){
        delete [] centroidS1[i];
        delete [] centroidS2[i];
    }
    free(centroidS1);
    free(centroidS2);

    delete [] of;
    delete [] Ks;
}

/**
 * Ejecuta la metaheurística con los datos dados.
 *
 * @param type Si se utilizará una función objetivo de
 *             maximización o de minimización.
 */
void GA::run(int type){
    int i;

    ////////////////////////////
    // Inicializaciones.
    initialize();

    //Inicializaciones de las funciones objetivo.
    for(i = 0; i < I; ++i){
        switch(type){
            case T_MAX:
                of[i] = foMax(i, Ks[i], metric);
                break;
            default:
                of[i] = foMin(i, Ks[i], metric);
        }
    }

    //Inicializaciones del mejor según el tipo de problema.
    float best;
    switch(type){
        case T_MAX:
            //Maximización.
            best = -1.0;
            break;
        default:
            //Minimización.
            best = numeric_limits<float>::infinity();
    }
    float last  = best;
    int count   = 0;

    int p1 = 0;
    int p2 = 0;
    int top;
    float cp, mut;

    ////////////////////////////
    // Algoritmo Genético.
    while(count < REPS){
        ////////////////////////////
        // Selección.
        selection(&p1, &p2, type);

        ////////////////////////////
        // Crossover.
        cp = ( (float)rand() )/( (float)RAND_MAX );
        if(cp > CP) continue;
        else{
            crossover(p1, p2, type);
            ////////////////////////////
            // Mutación.
            mut = ( (float)rand() )/( (float)RAND_MAX );
            if(mut > MUT) continue;
            else{
                mutation(type);
            }

            top = getBetter(type);
        }

        ////////////////////////////
        // Actualización del mejor.

        //Actualización de parámetros si es necesaria.
        updateBetter(top, &best, &last, &count, type);
    }

    bestDB = 1.0 / DB(bestSolution, bestCentroids, K);
}

/**
 * Selección de padres que se cruzarán.
 *
 * @param p1   Número de individuo del primer padre.
 * @param p2   Número de individuo del segundo padre.
 * @param type Tipo de funciones objetivo.
 */
void GA::selection(int* p1, int* p2, int type){
    torneo(p1, type);
    torneo(p2, type);

    return;
}

/**
 * Realiza un torneo para elegir a un padre.
 *
 * @param p    Número del padre elegido.
 * @param type Tipo de funciones objetivo.
 */
void GA::torneo(int* p, int type){
    int i;

    float best;
    int current;
    
    switch(type){
        case T_MAX:
            //Maximización.
            best = -1.0;
            break;
        default:
            //Minimización.
            best = numeric_limits<float>::infinity();
    }

    for(i = 0; i < torneoSize; ++i){
        current = rand() % I;

        switch(type){
            case T_MAX:
                //Maximización
                if(of[current] > best){
                    best = of[current];
                    *p = current;
                }
                break;
            default:
                //Minimización.
                if(of[current] < best){
                    best = of[current];
                    *p = current;
                }
        }
    }
}

/**
 * Mutación de algún individuo de la población.
 *
 * @param type Tipo de función objetivo.
 */
void GA::mutation(int type){
    int t = rand() % 3;
    int i = rand() % I;

    switch(t){
        case 0:
            crandom(i);            
            break;
        case 1:
            if(Ks[i] == Kmax) return;
            split(i);
            break;
        default:
            if(Ks[i] == 1) return;
            merge(i);
    }

    reassign(solution[i], centroid[i], Ks[i]);
    renamer(i, &Ks[i], size);

    switch(type){
        case T_MAX:
            of[i] = foMax(i, Ks[i], metric);
            break;
        default:
            of[i] = foMin(i, Ks[i], metric);
    }
}

/**
 * Perturba un centroide de manera aleatoria.
 *
 * @param i Número del individuo.
*/
void GA::crandom(int i){
    int j, rn;
    int cn = rand() % Ks[i];
    
    for(j = 0; j < M; ++j){
        rn = rand() % N;
        centroid[i][cn][j] = data[rn][j];
    }
}

/**
 * Agrega un nuevo cluster a partir de uno de los datos.
 *
 * @param i Número del individuo.
*/
void GA::split(int i){
    int j;
    int cn = rand() % N;

    for(j = 0; j < M; ++j)
        centroid[i][Ks[i]][j] = data[cn][j];

    Ks[i] += 1;
}

/**
 * Fusiona a dos clusters.
 *
 * @param i Número del individuo.
*/
void GA::merge(int i){
    int j;
    int cn1 = rand() % Ks[i];
    int cn2 = rand() % Ks[i];
    
    while(cn2 == cn1)
        cn2 = rand() % Ks[i];
    
    for(j = 0; j < M; ++j)
        centroid[i][cn2][j] = centroid[i][cn1][j];

    Ks[i] -= 1;
}

/**
 * Obtiene el mejor de la población.
 *
 * @param type Tipo de función objetivo.
 * @return Número de individuo mejor de la población.
 */
int GA::getBetter(int type){
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
void GA::initialize(){
    int i, j, l;
    int k;

    bool* done = new bool[N];
    for(i = 0; i < N; ++i)
        done[i] = false;

    for(i = 0; i < I ; ++i){
        for(j = 0; j < K; ++j){
            k = rand() % N;
            
            while(done[k])
                k = rand() % N;
                
            done[k] = true;

            for(l = 0; l < M; ++l)
                centroid[i][j][l] = data[k][l];

        }

        for(j = 0; j < N; j++)
            solution[i][j] = bestCluster(i, j);

        renamer(solution[i], centroid[i], &Ks[i], size);
    }

    delete [] done;
}

/**
 * Cruce de los padres. Los sustituye en caso de obtener
 * mejores hijos.
 *
 * @param p1   Número de individuo del primer padre.
 * @param p2   Número de individuo del segundo padre.
 * @param type Tipo de función objetivo.
 */
void GA::crossover(int p1, int p2, int type){
    int i, j;
    int index   = 0;
    int greater = Kmax;

    if(Ks[p1] <= Ks[p2]){
        index   = rand() % Ks[p1];
        greater = Ks[p1];
    }else{
        index   = rand() % Ks[p2];
        greater = Ks[p2];
    }
    int k1 = Kmax, k2 = Kmax;
    float of1, of2;
    bool p1_p2;

    ////////////////
    // Cruce.
    for(i = 0; i < index; ++i){
        for(j = 0; j < M; ++j){
            centroidS1[i][j] = centroid[p1][i][j];
            centroidS2[i][j] = centroid[p2][i][j];
        }
    }

    k1 = index;
    k2 = index;

    for(i = index; i < greater; ++i){
        for(j = 0; j < M; ++j){
            if(i < Ks[p1]){
                centroidS1[i][j] = centroid[p1][i][j];
                ++k1;
            }

            if(i < Ks[p2]){
                centroidS2[i][j] = centroid[p2][i][j];
                ++k2;
            }
        }
    }

    if(k1 > Kmax) k1 = Kmax;
    if(k2 > Kmax) k2 = Kmax;

    //Reasigna objetos a los clusters.
    reassign(s1, centroidS1, k1);
    reassign(s2, centroidS2, k2);

    //Cálculo de centroides.
    renamer(s1, centroidS1, &k1, size);
    renamer(s2, centroidS2, &k2, size);

    //Buscar repetidos y mutarlos.


    //Cálculo de la función objetivo.
    float pbest, sbest;
    int pbesti = 0, sbesti = 0;
    switch(type){
        case T_MAX:
            of1   = foMax(s1, centroidS1, k1, metric);
            of2   = foMax(s2, centroidS2, k2, metric);

            p1_p2 = p1 > p2;
            pbest = -1.0;
            sbest = -1.0;
            break;
        default:
            of1   = foMin(s1, centroidS1, k1, metric);
            of2   = foMin(s2, centroidS2, k2, metric);

            p1_p2 = p1 < p2;
            pbest = numeric_limits<float>::infinity();
            sbest = numeric_limits<float>::infinity();
    }

    /////////////////
    // Sustitución.
    float* temp = new float[4];
    temp[0] = of1;
    temp[1] = of2;
    temp[2] = of[p1];
    temp[3] = of[p2];

    for(i = 0; i < 4; ++i){
        switch(type){
            case T_MAX:
                if(temp[i] > pbest){
                    sbest = pbest;
                    pbest = temp[i];
                    sbesti = pbesti;
                    pbesti = i;
                }else{
                    if(temp[i] > sbest){
                        sbest = i;
                        sbesti = i;
                    }
                }
                break;
            default:
                if(temp[i] < pbest){
                    sbest = pbest;
                    pbest = temp[i];
                    sbesti = pbesti;
                    pbesti = i;
                }else{
                    if(temp[i] < sbest){
                        sbest = temp[i];
                        sbesti = i;
                    }
                }
        }
    }

    int p = 0;
    int* t;
    if(pbesti == 0 || pbesti == 1){
        if(p1_p2){
            t = solution[p2];
            switch(pbesti){
                case 0:
                    solution[p2] = s1;
                    s1 = t;
                    Ks[p2] = k1;
                    of[p2] = of1;
                    break;
                case 1:
                    solution[p2] = s2;
                    s2 = t;
                    Ks[p2] = k2;
                    of[p2] = of2;
            }
            p = 2;
        }else{
            t = solution[p1];
            switch(pbesti){
                case 0:
                    solution[p1] = s1;
                    s1 = t;
                    Ks[p1] = k1;
                    of[p1] = of1;
                    break;
                case 1:
                    solution[p1] = s2;
                    s2 = t;
                    Ks[p1] = k2;
                    of[p1] = of2;
            }
            p = 1;
        }
    }

    if(sbesti == 0 || sbesti == 1){
        if(p1_p2 && p != 1){
            t = solution[p2];
            switch(sbesti){
                case 0:
                    solution[p2] = s1;
                    s1 = t;
                    Ks[p2] = k1;
                    of[p2] = of1;
                    break;
                case 1:
                    solution[p2] = s2;
                    s2 = t;
                    Ks[p2] = k2;
                    of[p2] = of2;
            }
        }else if(!p1_p2 && p != 2){
            t = solution[p1];
            switch(sbesti){
                case 0:
                    solution[p1] = s1;
                    s1 = t;
                    Ks[p1] = k1;
                    of[p1] = of1;
                    break;
                case 1:
                    solution[p1] = s2;
                    s2 = t;
                    Ks[p1] = k2;
                    of[p1] = of2;
            }
        }
    }

    delete [] temp;
}
