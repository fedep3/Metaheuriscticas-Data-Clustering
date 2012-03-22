/*
Copyright (c) 2011 Alexander De Sousa, Federico Ponte

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
Software), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
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
 * @param _it  Número de iteraciones.
 * @param _w1  Peso de la distancia de los elementos
 *             dentro de un cluster en la función
 *             objetivo.
 * @param _w2  Peso de la distancia entre clusters
 *             en la función objetivo.
 * @param _w3  Peso del error en la función objetivo.
 *             en la función objetivo.
 * @param _zmx Máximo valor de un atributo.
 * @param _zmn Mínimo valor de un atributo.
 * @param _met Métrica.
 */
DE::DE(float** _d, int _m, int _n, int _k, int _i, int _it, float _w1,
       float _w2, float _w3, float *_zmx, float *_zmn)
: Metaheuristic(_d, _m, _n, _k, 0){

    int i, j;

    I = _i;
    maxit = _it;
    var = true;
    
    w1 = _w1;
    w2 = _w2;
    w3 = _w3;

    zmx = new float[M];
    zmn = new float[M];

    for(i = 0; i < M; ++i)
    {
        zmx[i] = _zmx[i];
        zmn[i] = _zmn[i];
    }

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
        centroid[i] = (float**) calloc(Kmax, sizeof(float*));
        if(centroid[i] == NULL) exit(1);
        for(j = 0; j < Kmax; ++j)
            centroid[i][j] = new float[M];
    }

    of = new float[I]; //Valor de las funciones objetivo.

    Ks = new int[I];

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
 * @param _w1  Peso de la distancia de los elementos
 *             dentro de un cluster en la función
 *             objetivo.
 * @param _w2  Peso de la distancia entre clusters
 *             en la función objetivo.
 * @param _w3  Peso del error en la función objetivo.
 *             en la función objetivo.
 * @param _zmx Máximo valor de un atributo.
 * @param _zmn Mínimo valor de un atributo.
 * @param _met Métrica.
 */
DE::DE(float** _d, int _m, int _n, int _k, int _i, int _it, float _Cr, 
       float _F, float _w1, float _w2, float _w3, float *_zmx, float *_zmn)
: Metaheuristic(_d, _m, _n, _k, 0){
    int i, j;

    I = _i;
    maxit = _it;
    F = _F;
    Cr = _Cr;
    var = false;

    w1 = _w1;
    w2 = _w2;
    w3 = _w3;

    zmx = new float[M];
    zmn = new float[M];

    for(i = 0; i < M; ++i)
    {
        zmx[i] = _zmx[i];
        zmn[i] = _zmn[i];
    }

    Zmax = 0.0;
    for(i = 0; i < M; ++i)
        Zmax += (zmx[i] - zmn[i]) * (zmx[i] - zmn[i]);
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
        centroid[i] = (float**) calloc(Kmax, sizeof(float*));
        if(centroid[i] == NULL) exit(1);
        for(j = 0; j < Kmax; ++j)
            centroid[i][j] = new float[M];
    }

    of = new float[I]; //Valor de las funciones objetivo.

    Ks = new int[I];

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

    delete [] Ks;
    delete [] of;
    delete [] zmx;
    delete [] zmn;
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
    float auxOf = 0;
    int m = 0, i = 0, j = 0, k = 0, l = 0, h = 0, p = 0;

    auxCent = (float**) calloc(Kmax, sizeof(float*));
    if(auxCent == NULL) exit(1);
    for(j = 0; j < Kmax; ++j)
        auxCent[j] = new float[M];
        
    auxSol = new int[N];

    initialize();

    for(i = 0; i < I; i++){

        assign(solution[i], centroid[i]);

        of[i] = foMin(solution[i], centroid[i], Ks[i]);

    }

    for(int it  = 0; it < maxit; it++){

        if(var){
            Cr = (CrMAX-CrMIN) * ((float) (maxit - it))/((float) maxit) + CrMIN;
            F = 0.5 * ( 1.0 + mtGetRandomFloat(drand));
        }

        for(i = 0; i < I; i++){

            m = randomInteger(0, I);

            while( m == i )
                m = randomInteger(0, I);

            k = randomInteger(0, I);

            while( k == m  || k == i)
                k = randomInteger(0, I);

            j = randomInteger(0, I);

            while( j == k || j == m || j == i)
                j = randomInteger(0, I);

            p = randomInteger(0, Kmax);

            for(l = 0; l < Kmax; l++){
                if(mtGetRandomFloat(drand) < Cr || l == p){
                    for(h = 0; h < M; h++){
                        auxCent[l][h] = centroid[m][l][h] + F*(centroid[j][l][h] - centroid[k][l][h]);
                        if(auxCent[l][h] > zmx[h] || auxCent[l][h] < zmn[h])
                            auxCent[l][h] = (zmx[h]-zmn[h]) * mtGetRandomFloat(drand) + zmn[h];
                    }
                }else{
                    for(h = 0; h < M; h++){
                        auxCent[l][h] = centroid[i][l][h];
                        if(auxCent[l][h] > zmx[h] || auxCent[l][h] < zmn[h])
                            auxCent[l][h] = (zmx[h]-zmn[h]) * mtGetRandomFloat(drand) + zmn[h];
                    }
                }
            }

            assign(auxSol, auxCent);

            if((auxOf = foMin(auxSol, auxCent, Kmax)) < of[i]){

                for(j = 0; j < N; j++)
                    solution[i][j] = auxSol[j];

                for(j = 0; j < Kmax; j++)
                    for(m = 0; m < M; m++)
                        centroid[i][j][m] = auxCent[j][m];

                of[i] = auxOf;
            }  

        }

    }

    for(i = 0; i < Kmax; i++)
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
    ++ofEval;
    return ( deFO(sol, cent, k) );
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

    int i, j, l, k;
    RandomArray r(N);

    for(i = 0; i < I ; ++i, r.reset(N)){

        Ks[i] = Kmax;

        for(j = 0; j < Kmax; ++j){
        
            k = r.get();

            for(l = 0; l < M; ++l)
                centroid[i][j][l] = data[k][l];

        }

    }

}

/**
 * Reasigna los elementos a cada cluster en un individuo.
 *
 * @param solution Solucion.
 * @param centroid Centroide.
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
                size[solution[largest]] -= 1;
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
                i = -1;
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

    res += w1 * dmax;

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

    res += w2 * ( Zmax - dmin );

    //Cálculo de Je.
    je = je / k;

    res += w3 * je;

    delete [] sums;

    return res;

}

/**
 * Reconstruye la solución.
 */
void DE::reconstruct(int type){

    printf("-- Reconstruyendo solución\n");

    int j,m;

    int best = getBetter(type);

    for(j = 0; j < N; j++)
        bestSolution[j] = solution[best][j];

    for(j = 0; j < Kmax; j++)
        for(m = 0; m < M; m++)
            bestCentroids[j][m] = centroid[best][j][m];

    bestFO = of[best];

}
