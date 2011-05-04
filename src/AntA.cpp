/**

 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108

 *
 * @section Descripción

 *
 * Clase concreta donde se definen las funciones y variables

 * necesarias para ejecutar el algoritmo hormiga.

 */

#include "AntA.h"

/**
 * Constructor de la clase AntA.

 *
 * @param _d   Datos del problema.
 * @param _m   Dimensión de cada dato.
 * @param _n   Cantidad de datos.

 * @param _k   Cantidad de clusters (iniciales).
 * @param _nA  Cantidad de hormigas.
 * @param _it  Cantidad de iteraciones.
 * @param _met Métrica.
 */
AntA::AntA(float** _d, int _m, int _n, int _nA, int _it, int _met)
: Metaheuristic(_d, _m, _n, _n, _met){
   
    nA = _nA;
    maxit = _it;
    ac = false;

    free = new int[N];
    cells = new vector<int>[N];    
    ants = new Ant[nA];

}

/**
 * Constructor de la clase AntA.

 *
 * @param _d      Datos del problema.
 * @param _m      Dimensión de cada dato.
 * @param _n      Cantidad de datos.
 * @param _k      Cantidad de clusters (iniciales).
 * @param _nA     Cantidad de hormigas.
 * @param _alpha2 Parametro usado para las probabilidades.

 * @param _it     Cantidad de iteraciones.
 * @param _met Métrica.
 */
AntA::AntA(float** _d, int _m, int _n, int _nA, float _alpha2, 
                                                int _it, int _met)
: Metaheuristic(_d, _m, _n, _n, _met){
   
    nA = _nA;
    maxit = _it;
    alpha2 = _alpha2;
    ac = true;

    free = new int[N];
    cells = new vector<int>[N];    
    ants = new Ant[nA];

}

/**
 * Destructor de la clase AntA.
 */
AntA::~AntA(){

    delete [] free;
    delete [] cells;
    delete [] ants;   

}

/**
 * Ejecuta la metaheurística con los datos dados. Además al final
 * de ella ejecuta un K-means, ya que la imagen que genera tiene
 * mucho ruido.
 *
 * @param type Si se utilizará una función objetivo de
 *             maximización o de minimización.

 */
void AntA::run(int type){

    int i = 0, ra = 0;

    initialize();
    
    for(i = 0; i < maxit; i++){

        ra = rand()%nA;
        
        if(ants[ra].free)
            pickAnt(ra);
        else
            dropAnt(ra);
        
    }

}

/*
 * Va a buscar los pixeles que no estan siendo cargados y la hormiga va a inten-
 * tar agarrarlo.
 *

 * @param Número de la hormiga.

 */
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

/*

 * Se encarga de armar la solutción a partir de la células y las hormigas.

 */
void AntA::reconstruct(int type){

    float actual = 0.0, max = -1.0;
    int i = 0, j = 0, best = 0, rp = 0;
    vector<int>::iterator it;

    printf("-- Reconstruyendo solución\n");

    //Suelto los píxeles que cargan las hormigas en la célula que más les
    //conviene.
    for(i = 0; i < nA; i++){
        if(!ants[i].free){
            max = -1.0;
            rp = ants[i].pixel;
            for(j = 0; j < N; j++){
                if(cells[j].size() >0){
                    if((actual = f(rp, j)) > max){
                        max = actual;
                        best = j;
                    }
                }
            }
            free[rp] = best;
            cells[best].push_back(rp);
            ants[i].drop(best);
        }
    }

    //Armó los clusters y sus centroides a partir de las células
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

        size[i] = 0;

    }

    for(i = 0; i < N; i++){

        for(j = 0; j < M; j++)
            bestCentroids[bestSolution[i]][j] += data[i][j];

        size[bestSolution[i]]++;

    }

    for(i = 0; i < K; i++)
        for(j = 0; j < M; j++)
            bestCentroids[i][j] = bestCentroids[i][j] / size[i];

    int m = 0;
    bool done[K];
    int change[K];

    //Procedo a agrupar mejor los clusters.
    if(K > 1){

        for(i = 0; i < K; i++){
            change[i] = i;
            done[i] = false;
        }

        //Procedor a compactar más.
        for(i = 0; i < K; i++){

            if(!done[i]){

                //Si es de sólo una elemento, buscó otro cluster que sea el más
                //parecido a él y los uno.
                if(size[i] == 1){

                    float min = numeric_limits<float>::infinity();
                    int mini = 0;

                    for(j = 0; j < K; j++){

                        if(i == j || done[j]) continue;

                        if( (actual = d(bestCentroids[i], bestCentroids[j])) < min){
                            min = actual;
                            mini = j;

                        }

                    }

                    change[mini] = i;

                    done[mini] = true;

                    for(m = 0; m < M; m++){

                        bestCentroids[mini][m] = bestCentroids[mini][m] * ((float) size[mini]);
                        bestCentroids[i][m]   += bestCentroids[mini][m];

                    }

                    size[i] += size[mini];

                    for(m = 0; m < M; m++)
                        bestCentroids[i][m] = bestCentroids[i][m]/((float) size[i]);


                //Sino procedo a buscar un cluster que se parezca y los uno con 
                //varios que se le parezcan.
                }else{

                    for(j = 0; j < K; j++){

                        if(i == j || done[j]) continue;

                        if((actual = d(bestCentroids[i], bestCentroids[j])) < alpha/4.0){

                            change[j] = i;

                            done[j] = true;

                            for(m = 0; m < M; m++){

                                bestCentroids[i][m]  = bestCentroids[i][m] * ((float) size[i]);
                                bestCentroids[j][m]  = bestCentroids[j][m] * ((float) size[j]);
                                bestCentroids[i][m] += bestCentroids[j][m];

                            }

                            size[i] += size[j];

                            for(m = 0; m < M; m++)
                                bestCentroids[i][m] = bestCentroids[i][m]/((float) size[i]);

                        }

                    }

                }

            }

        }

        //Renombro los clusters.
        for(i = 0; i < N; i++)
            bestSolution[i] = change[bestSolution[i]];

        //Los renombro de nuevo para que empiecen desde el 0 hasta el número de
        //clusters que se redujo.
        int index = 0;
        int v[K];

        for(i = 0; i < K; i++)
            v[i] = -1;
        
        for(i = 0; i < N; i++){

            if(v[ bestSolution[i] ] == -1){
                v[ bestSolution[i] ] = index;
                index++;
            }

            bestSolution[i] = v[ bestSolution[i] ];
        }

        for(i = 0; i < K; i++)
            if(v[i] != -1)
                for(m = 0; m < M; m++)
                    bestCentroids[v[i]][m] = bestCentroids[i][m];

        K = index;

    }

    bestFO = 1.0/(DB(bestSolution, bestCentroids, K));

}

/*

 * Procedimiento que se encarga de soltar el pixel de una hormiga. Funciona
 * del siguiente modo:
 *
 * Si (memoria de la hormiga tiene por lo menos 1)
 *     entonces Revisa la memoria local de la hormiga y elige la mejor 
 *         célula donde dejar, en caso que no logre soltar el
 *         pixel intenta en una ceĺula aleatoria
 * sino Dejar el pixel en una célula aleatoria

 *
 * @param ra Número de la hormiga que quiere soltar el pixel.
 */
void AntA::dropAnt(int ra){

    int j = 0, best = 0, rc = ants[ra].pixel, rp = ants[ra].pixel;
    int actual = 0;
    float max = -1.0, rn = 0.0;
    bool done = false;
    
    if(ants[ra].msize > 0){
    
        for( j = 0; j < ants[ra].msize; j++){
            rc =ants[ra].memory[j];
            if( (actual = f(rp, rc)) > max){
                max = actual;
                best = rc;
            }
        }

        rc = best;
        rn = ((float)rand())/((float)RAND_MAX);

        if(rn <= (1 - pow(cos(PIH * max),2 )) ){

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

/*
 * Calcula la probabilidad agarrar un pixel en la célula cell.
 *
 * @param pixel Pixel que se quiere agarrar.

 * @param cell Célula donde donde se encuentra.
 */
float AntA::ppick(int pixel, int cell){

    int size = cells[cell].size();

    if(size == 1)
        return 1.0;
    if(size == 2)
        return 0.7;
    else
        return pow(cos(PIH * f(pixel, cell)),2 );

}

/*
 * Calcula la probabilidad de dejar un pixel en la celula cell dada.

 *
 * @param pixel Pixel que se quiere soltar.
 * @param cell Célula donde se quiere soltar.
 */
float AntA::pdrop(int pixel, int cell){

    return 1 - pow(cos(PIH * f(pixel, cell)),2 );

}


/**
 * Funciones que devuelve el promedio de distancia entre el pixel y los
 * que se encuentra en la célula cell.
 *
 * @param pixel Pixel que se quiere soltar.
 * @param cell Célula donde se quiere soltar.
 */
float AntA::f(int pixel, int cell){
    ++ofEval;
    float sum = 0.0;
    vector<int>::iterator it;
    int size = cells[cell].size();

    if(size == 0) return 0.0;

    for ( it= cells[cell].begin() ; it < cells[cell].end(); it++ ){
        sum += alpha2/(alpha2 + pow(d(pixel, *it),2) );
    }
        
    sum = sum / size;

    return sum;
    
}

/**
 * Intercambio la posición f y s, del arreglo dado.
 *
 * @param array Arreglo a intercambiar.
 * @param f Primero posición.
 * @param s Segundo posición.
 */
inline void AntA::swap(int *array, int f, int s){

    int aux = array[f];
    array[f] = array[s];
    array[s] = aux;

}


/**

 * Inicializa las células donde las hormigas va a recoger y soltar
 * los pixeles. También hace que cada hormiga recoga un pixel.
 */
void AntA::initialize(){

    int i, k, c;
    int done[N];
    int size = N;

    for(i = 0; i < N; ++i)
        done[i] = i;

    for(i = 0; i < nA ; ++i){

        int pos = rand()%(size);

        k = done[pos];
        swap(done, pos, size-1);
        size--;

        ants[i].pick(k);
        free[k] = -1;

    }

    for(i = 0; i < size; i++){
        c = rand()%N;
        free[done[i]] = c;
        cells[c].push_back(done[i]);
    }

    if(!ac){
        printf("Calculando Alfa^2...\n");
        calcAlpha();
        ac = true;
        printf("Valor de Alfa^2: %.2f\n", alpha2);
    }
}

/*
 * Calcula el parámetro alpha2 usado en las probabilidades.
 */
void AntA::calcAlpha(){
    int i, j;
    double s = (double) (N * (N - 1));
    double atemp = 0.0;
    
    for(i = 0; i < N; ++i)
        for(j = i + 1; j < N; ++j)
            atemp += 2.0 * ( (double) d(i, j) );

    atemp = atemp / s;

    alpha = (float) atemp;

    atemp = atemp * atemp;

    alpha2 = (float) atemp;
}

void AntA::calcGFO(){
    bestDB = bestFO;
}
