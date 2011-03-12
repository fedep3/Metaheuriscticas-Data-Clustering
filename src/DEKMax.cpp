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

#include "DEKMax.h"

/**
 * Constructor de la clase DEKMax.
 *
 * @param _d   Datos del problema.
 * @param _m   Dimensión de cada dato.
 * @param _n   Cantidad de datos.
 * @param _k   Cantidad de clusters (iniciales).
 * @param _i   Cantidad de individuos.
 * @param _t   Número de iteraciones.
 * @param _met Métrica.
 */
DEKMax::DEKMax(float** _d, int _m, int _n, int _k, int _i, int _it, int _met)
: Metaheuristic(_d, _m, _n, _k, _met){
    int i, j;

    I = _i;

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

    // Se reserva la matriz de activación
    activation    = (float **) calloc(I, sizeof(float**));
    if(activation == NULL) exit(1);
    for(i = 0; i < I; ++i){
        activation[i] = (float*) calloc(K, sizeof(float*));
        if(activation[i] == NULL) exit(1);
    }

    maxit = _it;
    var = true;
}

/**
 * Constructor de la clase DEKMax.
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
DEKMax::DEKMax(float** _d, int _m, int _n, int _k, int _i, int _it, float _Cr, 
                                                   float _F, int _met)
: Metaheuristic(_d, _m, _n, _k, _met){
    int i, j;

    I = _i;

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

    // Se reserva la matriz de activación
    activation    = (float **) calloc(I, sizeof(float**));
    if(activation == NULL) exit(1);
    for(i = 0; i < I; ++i){
        activation[i] = (float*) calloc(K, sizeof(float*));
        if(activation[i] == NULL) exit(1);
    }

    maxit = _it;
    F = _F;
    Cr = _Cr;
    var = false;
}

/**
 * Destructor de la clase DEKMax.
 */
DEKMax::~DEKMax(){
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
    delete [] Ks;
}


/**
 * Ejecuta la metaheurística con los datos dados.
 *
 * @param type Si se utilizará una función objetivo de
 *             maximización o de minimización.
 */
void DEKMax::run(int type){

    float **auxCent;
    float *auxAct;
    int *auxSol;
    int auxK;
    int auxOf = 0, best = 0;

    int m = 0, i = 0, j = 0, k = 0, l = 0, h = 0;

    
    auxCent = (float**) calloc(Kmax, sizeof(float*));
    if(auxCent == NULL) exit(1);
    for(j = 0; j < Kmax; ++j)
        auxCent[j] = new float[M];
        
    auxAct = new float[Kmax];
    
    auxSol = new int[N];

    initialize();

    for(i = 0; i < I; i++){

        for(j = 0; j < N; j++)
            solution[i][j] = bestCluster(centroid[i], activation[i], j);

        stabilize(solution[i], centroid[i], activation[i]);

        Ks[i] = countClust(solution[i]);

        switch(type){
            case T_MAX:
                //Maximización.
                of[i] = foMax(solution[i], centroid[i], activation[i], metric);
                break;
            default:
                //Minimización.
                of[i] = foMin(solution[i], centroid[i], activation[i], metric);
        }

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

            for(l = 0; l < Kmax; l++){
                if(((float)rand())/((float)RAND_MAX) < Cr){
                    for(h = 0; h < M; h++)
                        auxCent[l][h] = centroid[m][l][h] + F*(centroid[j][l][h] - centroid[k][l][h]);
                    auxAct[l] = activation[m][l] + F*(activation[j][l] - activation[k][l]);

                    if( auxAct[l] > 1.0)
                        auxAct[l] = 1.0;
                    else if (auxAct[l] < 0.0)
                        auxAct[l] = 0.0;
                }else{
                    for(h = 0; h < M; h++)
                        auxCent[l][h] = centroid[i][l][h];
                    auxAct[l] = activation[i][l];
                }
            }

            for(j = 0; j < N; j++)
                auxSol[j] = bestCluster(auxCent, auxAct, j); 

            stabilize(auxSol, auxCent, auxAct);
            auxK = countClust(auxSol);

            switch(type){
                case T_MAX:
                    //Maximización.
                    if((auxOf = foMax(auxSol, auxCent, auxAct, metric)) > of[i]){

                        for(j = 0; j < N; j++)
                            solution[i][j] = auxSol[j];


                        for(j = 0; j < Kmax; j++)
                            for(m = 0; m < M; m++)
                                centroid[i][j][m] = auxCent[j][m];


                        for(j = 0; j < Kmax; j++)
                            activation[i][j] = auxAct[j];


                        of[i] = auxOf;
                        Ks[i] = auxK;

                    }                            
                    break;
                default:
                    //Minimización.
                    if((auxOf = foMin(auxSol, auxCent, auxAct, metric)) < of[i]){
                    
                        for(j = 0; j < N; j++)
                            solution[i][j] = auxSol[j];

                        for(j = 0; j < Kmax; j++)
                            for(m = 0; m < M; m++)
                                centroid[i][j][m] = auxCent[j][m];

                        for(j = 0; j < Kmax; j++)
                            activation[i][j] = auxAct[j];

                        of[i] = auxOf;
                        Ks[i] = auxK;
                    }  
            }

        }

    }

    best = getBetter(type);

    for(j = 0; j < N; j++)
        bestSolution[j] = solution[best][j];

    for(j = 0; j < Kmax; j++)
        for(m = 0; m < M; m++)
            bestCentroids[j][m] = centroid[best][j][m];

    K = Ks[best];

    renamer(bestSolution, &K);
    
    for(i = 0; i < Kmax; i++)
        delete [] auxCent[i];
    
    free(auxCent);
    
    delete [] auxSol;
    delete [] auxAct;
    
}

/**
 * Va a contar el número de centroides activados y va a devolver un
 * vector con la posiciones de los centroides activados, y el n
 * se coloca el número total de ellos.
 * @param activation Arreglo que contiene los valores de activación.
 * @param n Entero donde se va a colocar la cantidad de centroides
 *          activos.
 * @return Vector con las posiciones de los centroides activos.
 */
vector<int> *DEKMax::centsOn(float *activation, int &n){
    
    vector<int> *On = new vector<int>();
    n = 0;

    for(int i = 0; i < Kmax; i++){
        if(activated(activation, i)){
            On->push_back(i);
            n++;
        }
    }

    return On;

}

/**
 * Cuenta el número de clusters diferentes a los que pertenece un
 * datos.
 * @param solution Arreglo con la solución.
 * @return Cantidad de clusters diferentes.
 */
int DEKMax::countClust(int *solution){
    
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
 * Utiliza minimización de la métrica indicada. Si la métrica no
 * aparece, entonces retorna 0.0.
 *
 * @param sol  Vector solución.
 * @param cent Centroides asociados al vector solución.
 * @param activation Arreglo de los centroides activados.
 * @param met  Métrica.
 *
 * @return Valor de la solución con la métrica indicada.
 */
float DEKMax::foMin(int* sol, float** cent, float* activation, int met){
    switch(met){
        case M_DB:
            return DB(sol, cent, activation);
            break;
        case M_CS:
            return CS(sol, cent, activation);
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
 * @param activation Arreglo de los centroides activados.
 * @param met  Métrica.
 *
 * @return Valor de la solución con la métrica indicada.
 */
float DEKMax::foMax(int* sol, float** cent, float *activation, int met){
    switch(met){
        case M_DB:
            return (1.0/DB(sol, cent, activation));
            break;
        case M_CS:
            return (1.0/CS(sol, cent, activation));
            break;
        default:
            return 0.0;
    }
}


/**
 * Métrica DB. A menor valor de DB, mejor es la solución. Sólo va a tomar
 * en cuenta los centroides activados.
 *
 * @param sol  Vector solución.
 * @param cent Centroides asociados al vector solución.
 * @param activation Arreglo de los centroides activados.
 *
 * @return Valor de métrica.
 */
float DEKMax::DB(int* sol, float** cent, float *activation){
    int j, l, cluster;
    float m, t1, t2;


    float* S  = new float[Kmax];
    int* csize = new int[Kmax];

    for(j = 0; j < Kmax; ++j){
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
    for(j = 0; j < Kmax; ++j){
        switch(csize[j]){
            case 0:
                S[j] = S[j] / MIN_DIST;
                break;
            default:
                S[j] = S[j] / ( (float) csize[j] );
        }
    }

    //Cálculo de max R_{i,j}
    m = -1.0;
    for(j = 0; j < Kmax; ++j){
        if(activated(activation, j)){
            for(l = j + 1; l < Kmax; ++l){
                if(activated(activation, l)){
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
        }
    }


    delete [] S;
    delete [] csize;

    return ( m / ( (float) Kmax ) );
}

/**
 * Métrica CS. A menor valor de CS, mejor es la solución. Sólo va a tomar
 * en cuenta los centroides activados.
 *
 * @param sol  Vector solución.
 * @param cent Centroides asociados al vector solución.
 * @param activation Arreglo de los centroides activados.
 *
 * @return Valor de métrica.
 */
float DEKMax::CS(int* sol, float** cent, float *activation){
    float mn;
    float mx;
    float t;
    int j, l;
    bool first;

    float* MX  = new float[Kmax];
    int* csize  = new int[Kmax];

    for(j = 0; j < Kmax; ++j){
        MX[j]    = 0.0;
        csize[j]  = 0;
    }

    float denom = 0.0;
    float numer = 0.0;

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

    for(j = 0; j < Kmax; ++j){
        switch(csize[j]){
            case 0:
                MX[j] = MX[j] / MIN_DIST;
                break;
            default:
                MX[j] = ( MX[j] / ((float) csize[j]) );
        }
        numer += MX[j];
    }

    //Cálculo de denominador.
    for(j = 0; j < Kmax; ++j){
        if(activated(activation, j)){
            mn = numeric_limits<float>::infinity();
            for(l = 0; l < Kmax; ++l){
                if(j == l || activated(activation,l)) continue;

                t = d(cent[j], cent[l]);
                if(mn > t)
                    mn = t;
            }
            denom += mn;
        }
    }

    delete [] MX;
    delete [] csize;

    if(denom == 0.0)
        denom = MIN_DIST;

    return (numer / denom);
}

/**
 * Busca el mejor cluster para un objeto, tomando en cuanta los centroi-
 * des activados.
 *
 * @param centroid Centroides del individuo.
 * @param activation Arreglo con los valores de activación de los centroides.
 * @param e Objeto.
 *
 * @return Mejor cluster para el objeto.
 */
int DEKMax::bestCluster( float **centroid, float *activation, int e){

    int j;
    float mn = numeric_limits<float>::infinity();
    float t;
    int c = 0;
    for(j = 0; j < Kmax; ++j){
        if(activated(activation,j)){
            t = d(centroid[j], data[e]);
            if(mn > t){
                mn = t;
                c  = j;
            }
        }
    }
    return c;
    
}

/** Devuelve un booleano dependiendo si el valor de activacion e es mayor a 
  * 0.5.
  *
  * @param activation Arreglo con los valores de activación.
  * @param e Posición del valor.
  *
  * @ return booleano true si es mayor a 0.5, sino falso.
  */
bool DEKMax::activated(float *activation, int e){

    if(activation[e] > 0.5) return true;
    else return false;
    
}

/**
 * Intercambio la posición f y s, del arreglo dado.
 *
 * @param array Arreglo a intercambiar.
 * @param f Primero posición.
 * @param s Segundo posición.
 */
void DEKMax::swap(int *array, int f, int s){

    int aux = array[f];
    array[f] = array[s];
    array[s] = aux;

}

/**
 * Esta fución va a hacer dos cosas: 
 * 1.- Si se tienen menos de dos clusters, va hacer que halla dos valores de
 * activacion mayores que 0.5 y va a cambiar la solución colocando estos dos 
 * valores de forma aleatoria.
 * 2.- En el caso que halla cluster con menos de dos elementos, va a reiniciar 
 * toda la solucón de nuevo colocanto N/k elementos aleatorios en cada clusters.
 * donde N es la catidad total de datos y k la cantidad de clusters activos.
 * Finalmente si ocurrió la 1, 2 o ambas, se recalculan  los centroides.
 *
 * @param solution Arreglo solución.
 * @param centroid Matriz con cada uno de los centroides.
 * @param activation Arreglo con los valores de activación.
 */
void DEKMax::stabilize(int *solution, float** centroid, float *activation){

    int *count;
    bool lt2c = false, changed = false;
    int i = 0, j = 0, rn = 0, size = N;
    int k = 0, at = 0;
    int c = 0, r = 0;
    int ra[2], pos[2], v[N];
    vector<int> *On = centsOn(activation, k);

    //Arreglo si hay menos de 2 clusters.

    for(i = 0; i < 2 - k; i++){
        ra[i] = ((float)rand())/((float)RAND_MAX);

        while(ra[i] < 0.5)
            ra[i] = ((float)rand())/((float)RAND_MAX);

    }

    for(i = 0, j = 0; i < Kmax && j < (2 - k); i++){
        if(!activated(activation, i)){
            activation[i] = ra[j];
            j++;
        }
    }

    for(i = 0, j = 0; i < Kmax && j < 2; i++){
        if(activated(activation, i)){
            pos[j] = i;
            j++;
        }
    }

    if( k < 2){
        for(j = 0; j <N; j++){
            rn = rand()%2;
            solution[j] = pos[rn];
        }
        changed = true;

        delete On;

        k = 2;
        On = new vector<int>();
        On->push_back(pos[0]);
        On->push_back(pos[1]);
    }

    c = N/k;
    r = N%k;

    count = countE(solution);

    for( i = 0; i< Kmax; i++)
        if(activated(activation, i) && count[i]<2)
            lt2c = true;

    //Arreglo si hay un cluster con menos de dos elementos.

    if(lt2c){

        for(i = 0; i< N; i++)
            v[i] = i;

        for(i = 0; i < k; i++){
            at = On->at(i);
            count[at] = c;
            for(j = 0; j < c; j++){
                rn = rand() % size;
                solution[v[rn]] = at;
                swap(v, rn, size -1);
                size--;
            }
        }

        for(j = 0; j < r; j++){
            count[at]++;
            rn = rand() % size;
            solution[v[rn]] = at;
            swap(v, rn, size -1);
            size--;
        }

        changed = true;

    }

    //Si se cambia, hay que recalcular los centroides.

    if(changed){

        for(i = 0; i < Kmax; i++)
            if(activated(activation, i))
                for(j = 0; j < M; j++)
                    centroid[i][j] = 0.0;

        for(i = 0; i < N; i++)
            for(j = 0; j < M; j++)
                centroid[solution[i]][j] += data[i][j];

        for(i = 0; i < Kmax; i++)
            if(activated(activation, i))
                for(j = 0; j < M; j++)
                    centroid[i][j] = centroid[i][j]/((float)count[i]);

    }

    delete [] count;
    delete On;

}

/** Cuenta la cantidad de elementos de [0...Kmax] clusters. Retorna un arreglo
  * con la cantidad de cada uno.
  *
  * @param solution Arreglo solución.
  *
  * @return Arreglo con la cantidad de cada uno de los diversos clusters.
  */
int *DEKMax::countE(int *solution){

    int j = 0;
    int *result = new int[Kmax];
    
    for(j = 0; j < Kmax; j++)
        result[j] = 0;

    for(j = 0; j< N; j++)
        result[solution[j]]++;

    return result;

}

/**
 * Obtiene el mejor de la población.
 *
 * @param type Tipo de función objetivo.
 * @return Número de individuo mejor de la población.
 */
int DEKMax::getBetter(int type){
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
void DEKMax::initialize(){
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
            
            activation[i][j] = ((float)rand())/((float)RAND_MAX);

        }


    }

}
