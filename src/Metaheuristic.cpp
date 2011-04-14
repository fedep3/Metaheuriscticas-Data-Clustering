/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase abstracta en donde se definen las funciones y variables
 * básicas de una metaheurística.
 */

#include "Metaheuristic.h"

//////////////////////////////////
// Constructores y destructor.
////

/**
 * Constructor de la clase Metaheuristic.
 *
 * @param _d Datos del problema.
 * @param _m Dimensión de cada dato.
 * @param _n Cantidad de datos.
 * @param _k Cantidad de clusters (iniciales).
 */
Metaheuristic::Metaheuristic(double** _d, int _m, int _n, int _k, int _met){
    int i;
    data     = _d;
    M        = _m;
    N        = _n;
    K        = _k;
    Kmax     = K;
    metric   = _met;

    bestSolution = new int[N];

    bestCentroids = (double **) calloc(K, sizeof(double*));
    if(bestCentroids == NULL) exit(1);
    for(i = 0; i < K; ++i)
        bestCentroids[i] = new double[M];

    size = new int[K];
}

/**
 * Destructor de la clase Metaheuristic.
 */
Metaheuristic::~Metaheuristic(){
    int i;

    delete [] bestSolution;

    for(i = 0; i < Kmax; ++i)
        delete [] bestCentroids[i];
    free(bestCentroids);

    delete [] size;
}

//////////////////////////////
// Cálculos de distancias.
////

/**
 * Busca en la 'matriz' de distancia (en realidad es un arreglo
 * para ahorro de espacio) la distancia entre ambos objetos.
 *
 * @param i Primer objeto.
 * @param j Segundo objeto.
 *
 * @return Distancia entre los objetos.
 */
double Metaheuristic::d(int i, int j){
    if(i == j) return 0.0;

    switch(DEFAULT_DISTANCE){
        case COSINE_DISTANCE:
            return cosineDistance(data[i], data[j]);
        case EUCLIDEAN_DISTANCE:
            return euclideanDistance(data[i], data[j]);
    }
}

/**
 * Distancia entre dos vectores cualesquiera.
 *
 * @param v1 Primer vector.
 * @param v2 Segundo vector.
 *
 * @return Distancia entre los vectores.
 */
double Metaheuristic::d(double* v1, double* v2){
    switch(DEFAULT_DISTANCE){
        case COSINE_DISTANCE:
            return cosineDistance(v1, v2);
        case EUCLIDEAN_DISTANCE:
            return euclideanDistance(v1, v2);
    }
}

/**
 * Calcula la distancia del coseno entre dos vectores. Realmente,
 * el resultado no es la distancia del coseno, sino que es 1 menos
 * la distancia del coseno. De este modo, los resultados están a
 * corde con los de la distancia euclideana.
 *
 * @param v1 Primer vector.
 * @param v2 Segundo vector.
 *
 * @return Distancia del coseno entre los dos vectores dados.
 */
double Metaheuristic::cosineDistance(double* v1, double* v2){
    int k;
    double sum = 0.0;

    for(k = 0; k < M; ++k){
        sum += v1[k] * v2[k];
    }

    if(sum == 0.0) return euclideanDistance(v1, v2);

    return ( 1.0 - sum / ( norm(v1, M) * norm(v2, M) ) );
}

/**
 * Calcula la distancia euclideana entre dos vectores.
 * El cáculo debería hacerse con q = 2.0 de modo que se
 * calcule la distancia euclideana. Sin embargo, se puede
 * usar cualquier otra potencia.
 *
 * @param i Primer vector.
 * @param j Segundo vector.
 * @param q Exponente de la distancia.
 *
 * @return Distancia euclideana entre los dos vectores dados.
 */
double Metaheuristic::euclideanDistance(double* v1, double* v2, double q){
    int k;
    double sum = 0.0;
    double t;

    for(k = 0; k < M; ++k){
        t = v1[k] - v2[k];
        sum += pow(t, q);
    }

    return ( pow(sum, (1.0/q)) );
}

/**
 * Calcula la norma de un vector.
 *
 * @param vec Vector.
 * @param dim Dimensión del vector.
 * @param q   Exponente de la norma.
 *
 * @return Norma del vector.
 */
double Metaheuristic::norm(double* vec, int dim, double q){
    int k;
    double sum = 0.0;

    for(k = 0; k < dim; ++k){
        sum += pow(vec[k], q);
    }

    return ( pow(sum, (1.0/q)) );
}

/**
 * Busca el mejor cluster para un objeto.
 *
 * @param i Individuo.
 * @param e Objeto.
 *
 * @return Mejor cluster para el objeto.
 */
int Metaheuristic::bestCluster(int i, int e){
    return bestCluster(centroid[i], e);
}

/**
 * Busca el mejor cluster para un objeto.
 *
 * @param solution Solución.
 * @param centroids Centroides.
 * @param e Objeto.
 *
 * @return Mejor cluster para el objeto.
 */
int Metaheuristic::bestCluster(double **centroid, int e){
    int j;
    double mn = numeric_limits<double>::infinity();
    double t;
    int c = -1;
    for(j = 0; j < K; ++j){
        t = d(centroid[j], data[e]);
        if(mn > t){
            mn = t;
            c  = j;
        }
    }
    return c;
}

//////////////////////////////
// Métricas de calidad.
////

/**
 * Métrica DB. A menor valor de DB, mejor es la solución.
 *
 * @param i Individuo.
 * @param k Cantidad de clusters.
 *
 * @return Valor de métrica.
 */
double Metaheuristic::DB(int i, int k){
    return ( DB(solution[i], centroid[i], k) );
}

/**
 * Métrica DB. A menor valor de DB, mejor es la solución.
 *
 * @param sol  Vector solución.
 * @param cent Centroides asociados al vector solución.
 * @param k    Cantidad de clusters en el vector solución.
 *
 * @return Valor de métrica.
 */
double Metaheuristic::DB(int* sol, double** cent, int k){
    int j, l, cluster;
    double m, t1, t2;


    double* S  = new double[k];
    int* csize = new int[k];

    for(j = 0; j < k; ++j){
        S[j]    = 0.0;
        csize[j] = 0;
    }

    //Cálculo de S_i
    for(j = 0; j < N; ++j){
        cluster       =  sol[j];
        S[cluster]    += d(data[j], cent[cluster]);
        csize[cluster] += 1;
    }

    //Asegura que no haya división entre 0.
    for(j = 0; j < k; ++j){
        switch(csize[j]){
            case 0:
                S[j] = S[j] / MIN_DIST;
                break;
            default:
                S[j] = S[j] / ( (double) csize[j] );
        }
    }

    //Cálculo de max R_{i,j}
    m = -1.0;
    for(j = 0; j < k; ++j){
        for(l = j + 1; l < k; ++l){
            //Si dos centroides están a 0 distancia, probablemente
            //haya que unirlos.
            t1 = d(cent[j], cent[l]);
            if(t1 == 0.0){
                t1 = MIN_DIST;
            }

            t2 = ( (S[j] + S[l]) / t1 );
            if(m < t2){
                m = t2;
            }
        }
    }

    delete [] S;
    delete [] csize;

    return ( m / ( (double) k ) );
}

/**
 * Métrica CS. A menor valor de CS, mejor es la solución.
 *
 * @param i Individuo.
 * @param k Cantidad de clusters.
 *
 * @return Valor de métrica.
 */
double Metaheuristic::CS(int i, int k){
    return ( CS(solution[i], centroid[i], k) );
}

/**
 * Métrica CS. A menor valor de CS, mejor es la solución.
 *
 * @param sol  Vector solución.
 * @param cent Centroides asociados al vector solución.
 * @param k    Cantidad de clusters en el vector solución.
 *
 * @return Valor de métrica.
 */
double Metaheuristic::CS(int* sol, double** cent, int k){
    double mn;
    double mx;
    double t;
    int j, l;
    bool first;

    double* MX  = new double[k];
    int* csize  = new int[k];

    for(j = 0; j < k; ++j){
        MX[j]    = 0.0;
        csize[j]  = 0;
    }

    double denom = 0.0;
    double numer = 0.0;

    //Cálculo del numerador.
    first = true;
    for(j = 0; j < N; ++j){
        mx = 0.0;
        for(l = 0; l < N; ++l){
            if(first) csize[sol[l]] += 1;
            if(sol[j] == sol[l]){
                t = d(j, l);
                if(mx < t)
                    mx = t;
            }
        }
        MX[sol[j]] += mx;
        first = false;
    }

    for(j = 0; j < k; ++j){
        switch(csize[j]){
            case 0:
                MX[j] = MX[j] / MIN_DIST;
                break;
            default:
                MX[j] = ( MX[j] / ((double) csize[j]) );
        }
        numer += MX[j];
    }

    //Cálculo de denominador.
    for(j = 0; j < k; ++j){
        mn = numeric_limits<double>::infinity();
        for(l = 0; l < k; ++l){
            if(j == l) continue;

            t = d(cent[j], cent[l]);
            if(mn > t)
                mn = t;
        }
        denom += mn;
    }

    delete [] MX;
    delete [] csize;

    if(denom == 0.0)
        denom = MIN_DIST;

    return (numer / denom);
}

////////////////////////////
// Funciones objetivo.

/**
 * Utiliza minimización de la métrica indicada. Si la métrica no
 * aparece, entonces retorna 0.0.
 *
 * @param i   Individuo.
 * @param k   Cantidad de clusters.
 * @param met Métrica.
 *
 * @return Valor de la solución con la métrica indicada.
 */
double Metaheuristic::foMin(int i, int k, int met){
    switch(met){
        case M_DB:
            return DB(i, k);
            break;
        case M_CS:
            return CS(i, k);
            break;
        default:
            return 0.0;
    }
}

/**
 * Utiliza minimización de la métrica indicada. Si la métrica no
 * aparece, entonces retorna 0.0.
 *
 * @param sol  Vector solución.
 * @param cent Centroides asociados al vector solución.
 * @param k    Cantidad de clusters.
 * @param met  Métrica.
 *
 * @return Valor de la solución con la métrica indicada.
 */
double Metaheuristic::foMin(int* sol, double** cent, int k, int met){
    switch(met){
        case M_DB:
            return DB(sol, cent, k);
            break;
        case M_CS:
            return CS(sol, cent, k);
            break;
        default:
            return 0.0;
    }
}

/**
 * Utiliza maximización de la métrica indicada. Si la métrica no
 * aparece, entonces retorna 0.0.
 *
 * @param i   Individuo.
 * @param k   Cantidad de clusters.
 * @param met Métrica.
 *
 * @return Valor de la solución con la métrica indicada.
 */
double Metaheuristic::foMax(int i, int k, int met){
    switch(met){
        case M_DB:
            return (1.0/DB(i, k));
            break;
        case M_CS:
            return (1.0/CS(i, k));
            break;
        default:
            return 0.0;
    }
}

/**
 * Utiliza maximización de la métrica indicada. Si la métrica no
 * aparece, entonces retorna 0.0.
 *
 * @param sol  Vector solución.
 * @param cent Centroides asociados al vector solución.
 * @param k    Cantidad de clusters.
 * @param met  Métrica.
 *
 * @return Valor de la solución con la métrica indicada.
 */
double Metaheuristic::foMax(int* sol, double** cent, int k, int met){
    switch(met){
        case M_DB:
            return (1.0/DB(sol, cent, k));
            break;
        case M_CS:
            return (1.0/CS(sol, cent, k));
            break;
        default:
            return 0.0;
    }
}

///////////////////////
// Utilidades. 
///

/**
 * Reasigna los objetos a los centroides.
 *
 * @param sol   Vector solución.
 * @param cent  Centroides.
 * @param k     Cantidad de clusters.
 */
void Metaheuristic::reassign(int* sol, double** cent, int k){
    int i, j;
    int l = 0;
    double m;
    double t = 0.0;

    for(i = 0; i < N; ++i){
        m = numeric_limits<double>::infinity();
        for(j = 0; j < k; ++j){
            t = d(cent[j], data[i]);
            if(m > t){
                m = t;
                l = j;
            }
        }

        sol[i] = l;
    }
}

/**
 * Renombra los elementos de un vector.
 *
 * @param Individuo.
 * @param Cantidad de clusters del mismo.
 */
void Metaheuristic::renamer(int i, int* k){
    renamer(solution[i], k);
    return;
}

/**
 * Renombra los elementos de un vector.
 *
 * @param Vector solución.
 * @param Cantidad de clusters del mismo.
 */
void Metaheuristic::renamer(int* sol, int* k){
    int i;
    int index = 0;
    vector<int> v;
    
    for(i = 0; i < N; ++i){
        if( sol[i] + 1 > ( (int)v.size() )) v.resize(sol[i] + 1, -1);

        if(v[ sol[i] ] == -1){
            v[ sol[i] ] = index;
            ++index;
        }

        sol[i] = v[ sol[i] ];
    }

    *k = index;
}

/**
 * Renombra los elementos de un vector y calcula, además,
 * los centroides.
 *
 * @param Individuo.
 * @param Cantidad de clusters del mismo.
 *
 * @return Devuelve la cantidad de clusters vacíos removidos.
 */
int Metaheuristic::renamer(int i, int* k, int* csize){
    return ( renamer(solution[i], centroid[i], k, csize) );
}

/**
 * Renombra los elementos de un vector y calcula, además,
 * los centroides.
 *
 * @param Vector solución.
 * @param Cantidad de clusters del mismo.
 *
 * @return
 */
int Metaheuristic::renamer(int* sol, double** cent, int* k, int* csize){
    int i, j;
    int index = 0;
    vector<int> v;
    int tk = *k;

    //Inicialización del arreglo de sumas de promedio.
    double** sums = (double**) calloc(Kmax, sizeof(double*));
    for(i = 0; i < Kmax; ++i)
        sums[i] = new double[M];

    for(i = 0; i < Kmax; ++i){
        csize[i] = 0;
        for(j = 0; j < M; ++j)
            sums[i][j] = 0.0;
    }

    for(i = 0; i < N; ++i){
        //Renombramiento de clusters.
        if( sol[i] + 1 > ( (int)v.size() )) v.resize(sol[i] + 1, -1);
        if(v[ sol[i] ] == -1){
            v[ sol[i] ] = index;
            ++index;
        }

        sol[i] = v[ sol[i] ];

        //Cálculo de tamaño.
        csize[ sol[i] ] += 1;

        //Sumas de los promedios.
        for(j = 0; j < M; ++j)
            sums[ sol[i] ][j] += data[i][j];
    }

    //Actualización de clusters.
    for(i = 0; i < index; ++i)
        for(j = 0; j < M; ++j)
            cent[i][j] = sums[i][j] / ((double) csize[i]);

    //Actualización de la cantidad de clusters.
    *k = index;

    //Libera memoria.
    for(i = 0; i < Kmax; ++i)
        delete [] sums[i];
    free(sums);

    return (tk - *k); //Siempre es mayor o igual que cero.
}

/**
 * Actualiza el mejor encontrado en la población.
 *
 * @param i     Mejor individuo de la población.
 * @param best  Mejor valor heurístico de la función
 *              objetivo.
 * @param last  Último valor de la función objetivo.
 * @param count Cantidad de veces que se ha repetido una
 *              solución en las últimas iteraciones.
 * @param type  Tipo de función objetivo.
 */
void Metaheuristic::updateBetter(int i, double* best, double* last, int* count, int type){
        bool update;
        int j, l;

        //Determina si es necesario actualizar o no.
        switch(type){
            case T_MAX:
                //Maximización
                update = of[i] > (*best);
                break;
            default:
                //Minimización.
                update = of[i] < (*best);
        }

        //Actualización de parámetros si es necesaria.
        if(update){
            *best  = of[i];
            *last  = of[i];
            *count = 1;

            //Actualización de la mejor solución.
            for(j = 0; j < N; ++j)
                bestSolution[j] = solution[i][j];

            //Actualización del K.
            K = Ks[i];

            //Actualización de los mejores centroides.
            for(j = 0; j < K; ++j)
                for(l = 0; l < M; ++l)
                    bestCentroids[j][l] = centroid[i][j][l];

            //Actualización de la función objetivo.
            bestFO = of[i];

            update = false;
            return;
        }

        if(of[i] == (*last)){
            ++(*count);
        }

        //Actualiza el último usado.
        *last = of[i];
}

/**
 * Reconstruye la solución.
 */
void Metaheuristic::reconstruct(int type){
    bestDB = 1.0/(DB(bestSolution, bestCentroids, K));
}

