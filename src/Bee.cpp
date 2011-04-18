/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase concreta donde se definen las funciones y variables
 * necesarias para ejecutar el algoritmo de abeja.
 */
#include "Bee.h"

/**
 * Constructor de la clase GA.
 *
 * @param _d    Datos del problema.
 * @param _m    Dimensión de cada dato.
 * @param _n    Cantidad de datos.
 * @param _k    Cantidad de clusters (iniciales).
 * @param _i    Cantidad de individuos.
 * @param _ms   Cantidad de sitios seleccionados.
 * @param _es   Cantidad de sitios élite.
 * @param _eb   Cantidad de abejas a sitios élite.
 * @param _ob   Cantidad de abejas a sitios no-élite.
 * @param _met  Métrica.
 * @param _reps Repeticiones sin mejora.
 */
Bee::Bee(float** _d, int _m, int _n, int _k, int _i,
         int _ms, int _es, int _eb, int _ob,
         int _met, int _reps)
: Metaheuristic(_d, _m, _n, _k, _met){
    int i, j;
    I    = _i;
    REPS = _reps;

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

    ////////////////////////////////
    // Reservas para la población.

    bees    = new int[I];
    m_sites = _ms;
    e_sites = _es;
    o_sites = m_sites - e_sites;
    e_bees  = _eb;
    o_bees  = _ob;
}

/**
 * Destructor de la clase Bee.
 */
Bee::~Bee(){
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
    delete [] bees;
}


/**
 * Inicializa los centroides de los clusters.
 */
void Bee::initialize(){
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
 * Ejecuta la metaheurística con los datos dados.
 *
 * @param type Si se utilizará una función objetivo de
 *             maximización o de minimización.
 */
void Bee::run(int type){
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
            best = 0.0;
            break;
        default:
            //Minimización.
            best = numeric_limits<float>::infinity();
    }
    float last  = best;
    int count   = 0;

    int top = -1;

    ////////////////////////////
    // Algoritmo Bee.
    while(count < REPS){
        ////////////////////////////
        // Selección.
        selectSites(type);

        ////////////////////////////
        // Crossover.
        for(i = 0; i < I; ++i){
            neighbor(i, type);

            if(top != -1)
                top = getBetter(i, top, type);
            else
                top = i;
        }

        ////////////////////////////
        // Actualización del mejor.

        //Actualización de parámetros si es necesaria.
        updateBetter(top, &best, &last, &count, type);
    }

}

/**
 * Selecciona el mejor entre dos abejas.
 * 
 * @param i    Primera abeja.
 * @param j    Segunda abeja.
 * @param type Tipo de función objetivo.
 *
 * @return El índice de la mejor abeja.
 */
int Bee::getBetter(int i, int j, int type){
        switch(type){
            case T_MAX:
                //Maximización
                if(of[i] > of[j]) return i;
                break;
            default:
                //Minimización.
                if(of[i] < of[j]) return i;
        }
        return j;
}

/**
 * Selecciona los sitios que exploraran las abejas.
 *
 * @param type Tipo de función objetivo.
 */
void Bee::selectSites(int type){
    int i, j, r, s; 
    int* sites = new int[m_sites];
    for(i = 0; i < m_sites; ++i)
        sites[i]  = -1;

    bool* done  = new bool[I];
    for(i = 0; i < I; ++i)
        done[i]  = false;

    ////////////////////////////////////////////////
    //Llena el arreglo de sitios de manera ordenada.
    for(i = 0; i < m_sites; ++i){
        do
            r = rand() % I;
        while(done[r]);

        
        for(j = 0; j < m_sites; ++j){
            if(sites[j] == -1){
                sites[j] = r;
                j = m_sites;
                continue;
            }

            switch(type){
                case T_MAX:
                    //Maximización
                    if(of[r] > of[ sites[j] ]){
                        s = r;
                        r = sites[j];
                        sites[j] = s;
                    }
                    break;
                default:
                    //Minimización.
                    if(of[r] < of[ sites[j] ]){
                        s = r;
                        r = sites[j];
                        sites[j] = s;
                    }
            }
        }

        done[r] = true;
    }

    //////////////////////////////
    //Asigna labores a cada abeja.

    for(i = 0; i < I; ++i)
        bees[i] = -1;

    //Asignar sitios élite.
    for(i = 0; i < e_bees; ++i){
        do{
            r = rand() % I;
        }while(done[r]);

        j = rand() % e_sites;

        bees[r] = sites[j];
    }

    //Asignar sitios no-élite
    for(i = 0; i < o_bees; ++i){
        do{
            r = rand() % I;
        }while(done[r]);

        j = rand() % o_sites;

        if(e_sites + j >= m_sites)
            continue;

        bees[r] = sites[e_sites + j];
    }

    //Resto de las abejas se mueven aleatoriamente.
    delete [] sites;
    delete [] done;


    //////////////////////////////////////////////////////
    // Prepara a las abejas con la información necesaria.
    for(i = 0; i < I; ++i){
        if(bees[i] != -1){
            //Copiar solución.
            for(j = 0; j < N; ++j)
                solution[i][j] = solution[bees[i]][j];

            Ks[i] = Ks[bees[i]];

            //Copiar centroides.
            for(j = 0; j < Ks[i]; ++j)
                for(r = 0; r < M; ++r)
                    centroid[i][j][r] = centroid[bees[i]][j][r];
        }
    }
}

/**
 * Mueve a una abeja a un vecindario.
 *
 * @param a    Abeja.
 * @param type Tipo de función objetivo.
 */
void Bee::neighbor(int a, int type){
    int i;
    int r = rand() % N;
    int c = solution[a][r];

    for(i = 0; i < M; ++i)
        centroid[a][c][i] = data[r][i];

    for(i = 0; i < N; ++i)
        solution[a][i] = bestCluster(a, i);

    renamer(a, &Ks[a], size);

    switch(type){
        case T_MAX:
            of[a] = foMax(a, Ks[a], metric);
            break;
        default:
            of[a] = foMin(a, Ks[a], metric);
    }
}
