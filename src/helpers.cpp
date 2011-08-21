#include "helpers.h"

///////////////////////////////////////////
// Tiempo.

/**
 * Esctructura del tiempo.
 */
struct timeval t_p;

/**
 * Tiempo inicial.
 */
double tinit = 0.0;

/**
 * Tiempo final.
 */
double tend = 0.0;

/**
 * Tiempo de ejecución.
 */
double runtime = 0.0;

///////////////////////////////////////////
// Rquisitos genéricos

/**
 * Tipo de metaheurística.
 */
int algorithm = -1;
/**
 * Nombre del archivo de entrada.
 */
char* _input = NULL;
/**
 * Nombre del archivo de salida.
 */
char* _output = NULL;
/**
 * Tipo de lector.
 */
Reader* r = NULL;
/**
 * Metaheurística.
 */
Metaheuristic* m = NULL;

///////////////////////////////////////////
// Opciones requeridas por varios algoritmos

/**
 * Cantidad (máxima) de clusters.
 */
int _K = 0;

/**
 * Tipo de función objetivo.
 */
int _tf = T_MAX;
/**
 * Cantidad de repeticiones sin mejora.
 */
int _reps = 3;

///////////////////////////////////////////
// Requerimiento poblacionales (Ant, Bee, DE, GA y PSO)

/**
 * Individuos o partículas.
 */
int _I = 0;

///////////////////////////////////////////
// Requeridos Ant

/**
 * Promedio de diferencia entre todos los posibles par de vector de atributos.
 */
float _alpha = 0.0;

///////////////////////////////////////////
// Requeridos Bee

/**
 * Cantidad de sitios a explorar.
 */
int _m_sites = 0;

/**
 * Cantidad de sitios élite.
 */
int _e_sites = 0;

/**
 * Cantidad de abejas a sitios élite.
 */
int _e_bees = 0;

/**
 * Cantidad de abejas a sitios no-élite.
 */
int _o_bees = 0;

///////////////////////////////////////////
// Opciones DE

/**
 * Parámetro de escalado de los vectores.
 */
float _f = 0.0;

///////////////////////////////////////////
// Requeridos GA 

/**
 * Probabilidad de mutación.
 */
float _pm = 0.0;
/**
 * Tamaño del torneo.
 */
int _tt = 0;


///////////////////////////////////////////
// Requerido por DE y  opcional del GA

/**
 * Probabilidad de cruce.
 */
float _pc = 0.0;

///////////////////////////////////////////
// Requeridos PSO

/**
 * Constante de la componente cognitiva.
 */
float _c1 = 0.0;

/**
 * Constante de la componente social.
 */
float _c2 = 0.0;

/**
 * Peso inercial.
 */
float _W = 0.0;
/**
 * Velocidad máxima de las partículas.
 */
float _vmx = 0.0;


///////////////////////////////////////////
// Opcional PSO

/**
 * Si la función es weighted o no.
 */
bool weighted = false;

///////////////////////////////////////////
// Requeridos DE y PSO

/**
 * Máximos valores de cada atributo.
 */
float* _mxv = NULL;

/**
 * Mínimos valores de cada atributo.
 */
float* _mnv = NULL;

///////////////////////////////////////////
// Requeridos DE y opcional PSO

/**
 * Peso de la distancia intracluster.
 */
float _w1 = 0.0;

/**
 * Peso de la distancia intercluster;
 */
float _w2 = 0.0;

/**
 * Peso del error de la solución.
 */
float _w3 = 0.0;

///////////////////////////////////////////
// Variables de resultados.

//Estado actual (Algoritmo o Kmeans).
bool isInHibrid;

// Funciones objetivo.
float foalg;
float fohib;

// Evaluaciones de las funciones objetivo.
int evalalg;
int evalhib;

// Métricas de calidad del algoritmo.
float algDB;
float algJe;

// Métricas de calidad del híbrido.
float hibDB;
float hibJe;

/**
 * Función objetivo del algoritmo.
 */
float bestFO = 0.0;

/**
 * Función 1/DB(K) del algoritmo.
 */
float bestDB = 0.0;

/***/

/**
 * Si detiene definitivamente el programa o no.
 */
bool definitelyStopIt = false;

/**
 * Inicializa el contador.
 */
void initTime();

/**
 * Guarda el tiempo del contador.
 */
void endTime();

/**
 * Mejora la solución que ya se tiene al aplicar un Kmeans.
 *
 * @return Cantidad de evaluaciones de la función objetivo por
 *         parte del Kmeans.
 */
int improve();

/**
 * Inicializa los handlers del programa.
 */
void initHandlers();

/**
 * Restaura los handlers originales del programa.
 */
void retoreHandlers();

/**
 * Hace varias cosas antes de terminar el programa:
 * - Muestra el tiempo.
 * - Genera una imagen con la mejor solución.
 * - Dice la cantidad de clusters.
 * @param sig Señal.
 */
void longEnough(int);

/**
 * Hace varias cosas antes de terminar el programa:
 * - Muestra el tiempo.
 * - Genera una imagen con la mejor solución.
 * - Dice la cantidad de clusters.
 * @param sig Señal.
 */
void killIt(int);

/**
 * Estructuras que controla la señal de SIGINT.
 */
struct sigaction new_sigint, old_sigint;

/**
 * Estructuras que controla la señal de SIGALRM.
 */
struct sigaction new_sigalrm, old_sigalrm;

/**
 * Mejora la solución que ya se tiene al aplicar un Kmeans.
 *
 * @return Cantidad de evaluaciones de la función objetivo por
 *         parte del Kmeans.
 */
int improve(){
    int i, j;

    Kmeans *KA = new Kmeans(m->data, m->M, m->N, m->K, M_DB, 3);

    KA->setCentroids(m->bestSolution, m->bestCentroids, _tf);

    KA->run(_tf);

    for(i = 0; i < KA->N; ++i)
        m->bestSolution[i] = KA->bestSolution[i];

    for(i = 0; i < KA->K; ++i)
        for(j = 0; j < KA->M; ++j)
            m->bestCentroids[i][j] = KA->bestCentroids[i][j];

    m->K = KA->K;

    m->bestFO = KA->bestFO;

    i = KA->ofEval;

    delete KA;

    return i;
}

/**
 * Inicializa los handlers del programa.
 */
void initHandlers(){
	memset(&new_sigint, 0, sizeof(new_sigint));
	memset(&old_sigint, 0, sizeof(old_sigint));
	memset(&new_sigalrm, 0, sizeof(new_sigalrm));
	memset(&old_sigalrm, 0, sizeof(old_sigalrm));

    new_sigint.sa_handler = killIt;
    sigemptyset(&new_sigint.sa_mask);
    new_sigint.sa_flags   = SA_NODEFER;
    sigaction(SIGINT, &new_sigint, &old_sigint);

    new_sigalrm.sa_handler = longEnough;
    sigemptyset(&new_sigalrm.sa_mask);
    new_sigalrm.sa_flags = SA_NODEFER;
	sigaction(SIGALRM, &new_sigalrm, &old_sigalrm);

	alarm(MAXRUNTIME);
}

/**
 * Restaura los handlers originales del programa.
 */
void restoreHandlers(){

    if(definitelyStopIt){

        printf("-- Tiempo excedido...\n");

        printf("-- Cantidad de Clusters Final: %d\n", m->K);

        printf("** Algoritmo\n");

        printf("-- Cantidad de evaluaciones de la función objetivo del algoritmo: %d\n", evalalg);

        printf("-- Valor de la función objetivo del algoritmo: %.4f\n", foalg);

        printf("-- Índice DB del algoritmo: %.4f\n", algDB);

        printf("-- Error Je del algoritmo: %.4f\n", algJe);

        r->write(_output, m->bestSolution, m->K);

        exit(0);
    }

    definitelyStopIt = true;

    sigemptyset(&old_sigint.sa_mask);
    old_sigint.sa_handler = SIG_DFL;
    old_sigint.sa_flags   = SA_NODEFER;
    sigaction(SIGINT, &old_sigint, NULL);

    new_sigalrm.sa_handler = longEnough;
    sigemptyset(&new_sigalrm.sa_mask);
    new_sigalrm.sa_flags = SA_NODEFER;
    sigaction(SIGALRM, &new_sigalrm, &old_sigalrm);

    alarm(MAXRUNTIME);
}
/**
 * Hace varias cosas antes de terminar el programa:
 * - Muestra el tiempo.
 * - Genera una imagen con la mejor solución.
 * - Dice la cantidad de clusters.
 * @param sig Señal.
 */
void longEnough(int sig){
    try{
        restoreHandlers();

        endTime();

        printf("-- Tiempo excedido...\n");

        if(!isInHibrid){
            m->reconstruct(_tf);

            evalalg = m->ofEval;

            foalg = m->bestFO;

            algDB = m->calcDB();
            algJe = m->calcJe();
        }

        printf("-- Cantidad de Clusters Final: %d\n", m->K);

        printf("** Algoritmo\n");

        printf("-- Cantidad de evaluaciones de la función objetivo del algoritmo: %d\n", evalalg);

        printf("-- Valor de la función objetivo del algoritmo: %.4f\n", foalg);

        printf("-- Índice DB del algoritmo: %.4f\n", algDB);

        printf("-- Error Je del algoritmo: %.4f\n", algJe);

        r->write(_output, m->bestSolution, m->K);

        exit(0);
    }catch(exception& e){
        cout << e.what() << endl;
        exit(1);
    }
}

/**
 * Hace varias cosas antes de terminar el programa:
 * - Muestra el tiempo.
 * - Genera una imagen con la mejor solución.
 * - Dice la cantidad de clusters.
 * @param sig Señal.
 */
void killIt(int sig){
    try{
        restoreHandlers();

        if(!isInHibrid){
            endTime();

            m->reconstruct(_tf);

            evalalg = m->ofEval;

            foalg = m->bestFO;

            algDB = m->calcDB();
            algJe = m->calcJe();

            if(algorithm != M_KMEANS){

                printf("-- Mejorando solución...\n");

                initTime();

                isInHibrid = true;
                evalhib = improve();

                endTime();

                fohib = m->bestFO;

                hibDB = m->calcDB();
                hibJe = m->calcJe();
            }
        }else{
            endTime();

            fohib = m->bestFO;

            hibDB = m->calcDB();
            hibJe = m->calcJe();
        }

        printf("-- Cantidad de Clusters Final: %d\n", m->K);

        printf("** Algoritmo\n");

        printf("-- Cantidad de evaluaciones de la función objetivo del algoritmo: %d\n", evalalg);

        printf("-- Valor de la función objetivo del algoritmo: %.4f\n", foalg);

        printf("-- Índice DB del algoritmo: %.4f\n", algDB);

        printf("-- Error Je del algoritmo: %.4f\n", algJe);

        if(algorithm != M_KMEANS){

            printf("** Híbrido\n");

            printf("-- Cantidad de evaluaciones de la función objetivo del Kmeans: %d\n", evalhib);

            printf("-- Cantidad de evaluaciones de la función objetivo del híbrido: %d\n", (evalalg + evalhib));

            printf("-- Valor de la función objetivo del híbrido: %.4f\n", fohib);

            printf("-- Índice DB del híbrido: %.4f\n", hibDB);

            printf("-- Error Je del híbrido: %.4f\n", hibJe);
        }

        r->write(_output, m->bestSolution, m->K);

        exit(0);
    }catch(exception& e){
        cout << e.what() << endl;
        exit(1);
    }
}

/**
 * Inicializa el contador.
 */
void initTime(){
    if (!gettimeofday(&t_p, NULL)){
        tinit = (double) t_p.tv_sec +
                ((double) t_p.tv_usec)/1000000.0;
    }else{
        fprintf(stderr, "-- Problema con el contador de tiempo\n");
        exit(1);
    }
}

/**
 * Guarda el tiempo del contador.
 */
void endTime(){
    if (!gettimeofday(&t_p,NULL)){
        tend = (double) t_p.tv_sec +
              ((double) t_p.tv_usec)/1000000.0;
    }else{
        fprintf(stderr, "-- Problema con el contador de tiempo\n");
        exit(1);
    }

    runtime = tend - tinit;
    printf("-- Tiempo de corrida: %1.4f segs.\n", runtime);
}

#define V_MAX 0
int mxs = 0;
#define V_MIN 1
int mns = 0;
/**
 * Parsea el vector de acuerdo a un string.
 */
void parseVector(int t){
    int i;
    int vs;
    vector<float> v;
    float* temp;

    stringstream line(optarg);
    string str;
    while(getline(line, str, ',')){
        v.push_back( atof( str.c_str() ) );
    }

    vs = (int) v.size();

    switch(t){
        case V_MAX:
            _mxv = new float[vs];
            mxs  = vs;
            temp = _mxv;
            break;
        case V_MIN:
            _mnv = new float[vs];
            mns  = vs;
            temp = _mnv;
            break;
        default:
            exit(1);
    }
    
    for(i = 0; i < vs; ++i)
        temp[i] = v[i];
}

void help(){

    printf("Uso:\n\
	./mhs [-?|--help] <Requeridas Genéricas> <Opciones Del Algoritmo>\n\
\n\
Opciones permitidas:\
\n\
Ayuda:\n\
  -? [ --help ]         Produce mensaje de ayuda.\n\
\n\
Requeridas genéricas:\n\
  --a arg               Algoritmo a ejecutar:\n\
                        Ant\n\
                        Bee\n\
                        DE\n\
                        GA\n\
                        Kmeans\n\
                        PSO\n\
  --fi arg              Archivo de entrada.\n\
  --fo arg              Archivo de salida.\n\
  --t arg               Tipo de archivo de entrada (CSV, TIFF o PNG).\n\
\n\
Opciones requeridas por varios algorimos:\n\
  --k arg               Número de clusters (Todos menos Ant).\n\
  --reps arg (=3)       Número de iteracion en el caso de Ant y DE, y canti-\n\
                        dad de iteraciones que no se mejora la solucion\n\
                        actual en el caso de Bee, GA, Kmeans y PSO. El valor\n\
                        por default es 3.\n\
  --tf arg (=MAX)       Si se desea maximizar o minimizar (MAX o MIN), el MAX\n\
                        está por defecto (Bee, GA y Kmeans).\n\
\n\
Requeridos poblacionales (Ant, Bee, DE, GA y PSO):\n\
  --i arg               Cantidad de individuos.\n\
\n\
Opciones Ant:\n\
  --alpha arg           Promedio de diferencia entre todos los posibles par\n\
                        de vector de atributos.\n\
\n\
Requeridos Bee:\n\
  --e arg               Cantidad de parches élite.\n\
  --eb arg              Cantidad de abejas a parches élite.\n\
  --m arg               Cantidad de parches de exploración.\n\
  --ob arg              Cantidad de abejas a parches no-élite.\n\
\n\
Opciones DE:\n\
  --f arg               Párametro escalar, requiere que el pc también este.\n\
                        establecido, sino se puede dejar de colocar ambos.\n\
\n\
Requeridos GA:\n\
  --pm arg              Probabilidad de mutación.\n\
  --tt arg              Tamaño del torneo.\n\
\n\
Requerido por GA y opcional DE:\n\
  --pc arg              Probabilidad de cruce, requerida en el genético,\n\
                        opcional en el DE (requiere el factor escalar esté\n\
                        establecido también).\n\
\n\
Requeridos PSO:\n\
  --c1 arg              Constante de la componente cognitiva.\n\
  --c2 arg              Constante de la componente social.\n\
  --w arg               Peso inercial.\n\
  --vmx arg             Escalar que acompaña al vector de velocidad máxima.\n\
\n\
Requeridos DE y PSO:\n\
  --mn arg              Vector de valores mínimos de cada atributo.\n\
  --mx arg              Vector de valores máximos de cada atributo.\n\
\n\
Requeridos DE y opcional PSO:\n\
  --w1 arg              Peso de la distancia intracluster.\n\
  --w2 arg              Peso de la distancia intercluster.\n\
  --w3 arg              Peso del error.La suma debe ser w1+w2+w3 = 1.0.\n\n\n\
Ejempos:\n\
\n\
- Ant\n\
\n\
    * ./mhs --t PNG --fi entrada.png --fo salida.png --a Ant --i 20 --reps \
100\n\
    * ./mhs --t PNG --fi entrada.png --fo salida.png --a Ant --i 30 --reps \
9000 --alpha 244.5\n\
\n\
- Bee\n\
\n\
    * ./mhs --t CSV --fi entrada.csv --fo salida --a Bee --e 9 --eb 10 --ob 9\
 --m 10 --i 40 --k 3 --reps 10\n\
\n\
- DE\n\
\n\
    * ./mhs --a DE --k 2 --fi entrada.tiff --fo salida.png --t TIFF --i 20 \
--w1 0.33 --w2 0.33 --w3 0.33 --mn 0.0,0.0,0.0 --mx 255.0,255.0,255.0 --reps \
30\n\
    * ./mhs --a DE --k 2 --fi entrada.png --fo salida.png --t TIFF --i 20 --w1 \
0.2 --w2 0.4 --w3 0.4 --mn 0.0,0.0,0.0 --mx 255.0,255.0,255.0 --f 0.2 --pc 0.4 \
--reps 30\n\
\n\
- GA \n\
\n\
    * ./mhs --a GA --k 2 --fi entrada.tiff --fo salida.png --t TIFF --i 20 --pc\
 0.8 --pm 0.1 --tt 5\n\
\n\
- Kmeans\n\
\n\
    * ./mhs --a Kmeans --k 2 --fi entrada.png --fo salida.png --t PNG --tf \
MIN\n\
\n\
- PSO\n\
    * ./mhs --t CSV --fi entrada.csv --fo salida --a PSO --c1 1.5 --c2 1.5 \
--W 3.0 --vmx 0.2 --mn 0.0,0.0,0.0 --mx 255.0,255.0,255.0 --i 20 --k 5\n\
    * ./mhs --t PNG --fi entrada.png --fo salida.png --a PSO --c1 1.5 --c2 1.5 \
--W 3.0 --vmx 0.5 --mn 0.0,0.0,0.0 --mx 255.0,255.0,255.0 --i 20 --k 5 --w1 \
0.33 --w2 0.33 --w3 0.33\n\n\n");

}

/**
 * Lee los argumentos del programa e inicializa las estructuras.
 *
 * @param argc Cantidad de Argumentos.
 * @param argv Argumentos.
 */
void initIt(int argc, char* argv[]){

    int c;
    int option_index; 

    bool* optgen = new bool[4];
    for(c = 0; c < 4; ++c) optgen[c] = false;
    bool* optextra = new bool[3];
    for(c = 0; c < 3; ++c) optextra[c] = false;
    bool  optindiv  = false;
    bool  optant  = false;
    bool* optbee  = new bool[4];
    for(c = 0; c < 4; ++c) optbee[c] = false;
    bool optde = false;
    bool* optga  = new bool[2];
    for(c = 0; c < 2; ++c) optga[c] = false;
    bool optdega = false;
    bool* optpso  = new bool[4];
    for(c = 0; c < 4; ++c) optpso[c] = false;
    bool* optdepso  = new bool[2];
    for(c = 0; c < 2; ++c) optdepso[c] = false;
    bool* optdeopso  = new bool[3];
    for(c = 0; c < 3; ++c) optdeopso[c] = false;
    bool noerror = true;
    bool aux = false;

    while(true){

        static struct option long_options[] =
        {
            /* Ayuda */
            {"help", no_argument, 0, '?'},

            /* Requeridas genéricas */
            {"a",  required_argument, 0, 'A'},
            {"fi", required_argument, 0, 'B'},
            {"fo", required_argument, 0, 'C'},
            {"t",  required_argument, 0, 'D'},

            /* Opciones requeridas por varios algorimos */
            {"k",  required_argument, 0, 'E'},
            {"reps", required_argument, 0, 'F'},
            {"tf", required_argument, 0, 'G'},

            /* Requeridos poblacionales (Ant, Bee, DE, GA y PSO) */
            {"i",  required_argument, 0, 'H'},

            /* Opciones Ant */
            {"alpha", required_argument, 0, 'I'},

            /* Opciones Bee */
            {"m", required_argument, 0,'J'},
            {"e", required_argument, 0,'K'},
            {"eb", required_argument, 0,'L'},
            {"ob", required_argument, 0,'M'},

            /* Opciones DE */
            {"f", required_argument, 0,'N'},

            /* Requeridos GA */
            {"pm", required_argument, 0, 'O'},
            {"tt", required_argument, 0, 'P'},

            /* Requerido por GA y opcional DE */
            {"pc", required_argument, 0, 'Q'},

            /* Requeridos PSO */
            {"c1", required_argument, 0, 'R'},
            {"c2", required_argument, 0, 'S'},
            {"W",  required_argument, 0, 'T'},
            {"vmx", required_argument, 0,'U'},

            /* Requeridos DE y PSO */
            {"mx", required_argument, 0, 'V'},
            {"mn", required_argument, 0, 'W'},

            /* Requeridos DE y opcional PSO */
            {"w1", required_argument, 0, 'X'},
            {"w2", required_argument, 0, 'Y'},
            {"w3", required_argument, 0, 'Z'}
        };

        option_index = 0;

        c = getopt_long(argc, argv,
                        "?A:B:C:D:E:F:G:H:I:J:K:L:M:N:O:P:Q:R:S:TU:V:W:X:Y:Z:d",
                        long_options, &option_index);

        if(c == -1)
            break;

        switch(c){
            case '?':
                help();
                exit(1);
                break;
            case 'A':
                if(strcmp(optarg, "Kmeans") == 0){
                    algorithm = M_KMEANS;
                }else if(strcmp(optarg, "GA") == 0){
                    algorithm = M_GA;
                }else if(strcmp(optarg, "PSO") == 0){
                    algorithm = M_PSO;
                }else if(strcmp(optarg, "DE") == 0){
                    algorithm = M_DE;
                }else if(strcmp(optarg, "Ant") == 0){
                    algorithm = M_ANT;
                }else if(strcmp(optarg, "Bee") == 0){
                    algorithm = M_BEE;
                }else{
                    fprintf(stderr, "Debe elegir un algoritmo válido.\n");
                    fprintf(stderr, "Kmeans, GA, PSO, DE, Ant o Bee son válidos\
, %s no lo es.\n", optarg);
                    noerror = false;
                }

               optgen[0] = true;

                break;
            case 'B':
                _input = optarg;

               optgen[1] = true;

                break;
            case 'C':
                _output = optarg;

               optgen[2] = true;

                break;
            case 'D':
                if(strcmp(optarg, "PNG") == 0){
                    r = new Png();
                }else if(strcmp(optarg, "TIFF") == 0){
                    r = new Tiff();
                }else if(strcmp(optarg, "CSV") == 0){
                    r = new Csv();
                }else{
                    fprintf(stderr, "Debe elegir un tipo de archivo válido.\n");
                    fprintf(stderr, "PNG, TIFF o CSV son válidos, %s no lo es.\
\n", optarg);
                    noerror = false;
                }

               optgen[3] = true;

                break;
            case 'E':
                _K = atoi(optarg);
                if(_K < 2){
                    fprintf(stderr, "La cantidad de clusters debe ser mayor que\
 2.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optextra[0] = true;

                break;
            case 'F':
                _reps = atoi(optarg);
                if(_reps <= 0){
                    fprintf(stderr, "La cantidad de repeticiones debe ser mayor\
 que 0.\n");
                    fprintf(stderr, "%s no lo es. Se utilizará 3 por defecto.\
\n", optarg);
                    _reps = 3;
                }

                optextra[1] = true;

                break;
            case 'G':
                if(strcmp(optarg, "MAX") == 0){
                    _tf = T_MAX;
                }else if(strcmp(optarg, "MIN") == 0){
                    _tf = T_MIN;
                }else{
                    fprintf(stderr, "Debe elegir un tipo de función válido.\n");
                    fprintf(stderr, "MAX ó MIN son válidos, %s no lo es.\n", 
                                                                        optarg);
                    noerror = false;
                }

                optextra[2] = true;

                break;
            case 'H':
                _I = atoi(optarg);
                if(_I <= 0){
                    fprintf(stderr, "La cantidad de individuos debe ser mayor \
que 0.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optindiv = true;

                break;
            case 'I':
                _alpha = atof(optarg);
                if(_alpha < 0.0){
                    fprintf(stderr, "El alpha debe ser positivo.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optant = true;

                break;
            case 'J':
                _m_sites = atoi(optarg);
                if(_m_sites < 0){
                    fprintf(stderr, "El m debe ser positivo.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optbee[0] = true;

                break;
            case 'K':
                _e_sites = atoi(optarg);
                if(_e_sites < 0){
                    fprintf(stderr, "El e debe ser positivo.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optbee[1] = true;

                break;
            case 'L':
                _e_bees = atoi(optarg);
                if(_e_bees < 0){
                    fprintf(stderr, "El eb debe ser positivo.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optbee[2] = true;

                break;
            case 'M':
                _o_bees = atoi(optarg);
                if(_o_bees < 0){
                    fprintf(stderr, "El ob debe ser positivo.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optbee[3] = true;

                break;
            case 'N':
                _f = atof(optarg);
                if(_f < 0.0 || _f > 1.0){
                    fprintf(stderr, "El parámetro de escalado f debe estar \
entre 0.0 y 1.0\n");
                    fprintf(stderr, "%s no lo está.\n", optarg);
                    noerror = false;
                }

                optde = true;

                break;  
            case 'O':
                _pm = atof(optarg);
                if(_pm < 0.0 && _pm > 1.0){
                    fprintf(stderr, "El porcentaje de mutación debe estar entre\
 0.0 y 1.0.\n");
                    fprintf(stderr, "%s no lo está.\n", optarg);
                    noerror = false;
                }
                optga[0] = true;

                break;
            case 'P':
                _tt = atoi(optarg);
                if(_tt <= 0){
                    fprintf(stderr, "El tamaño del torneo debe ser mayor que\
 0.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optga[1] = true;

                break;
            case 'Q':
                _pc = atof(optarg);
                if(_pc < 0.0 && _pc > 1.0){
                    fprintf(stderr, "El porcentaje de cruce debe estar entre \
0.0 y 1.0.\n");
                    fprintf(stderr, "%s no lo está.\n", optarg);
                    noerror = false;
                }

                optdega = true;

                break;
            case 'R':
                _c1 = atof(optarg);
                if(_c1 <= 0.0){
                    fprintf(stderr, "El c1 debe mayor que 0.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optpso[0] = true;

                break;
            case 'S':
                _c2 = atof(optarg);
                if(_c2 <= 0.0){
                    fprintf(stderr, "El c2 debe mayor que 0.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optpso[1] = true;

                break;
            case 'T':
                _W = atof(optarg);
                if(_W <= 0.0){
                    fprintf(stderr, "El W debe ser mayor que 0.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optpso[2] = true;

                break;
            case 'U':
                _vmx = atof(optarg);
                if(0.1 > _vmx || _vmx > 1.0){
                    fprintf(stderr, "El vmx debe estar en el rango de 0.1 a 1.0\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optpso[3] = true;

                break;
            case 'V':
                parseVector(V_MIN);

                optdepso[0] = true;

                break;
            case 'W':
                parseVector(V_MAX);

                optdepso[1] = true;

                break;
            case 'X':

                _w1 = atof(optarg);
                if(_w1 < 0.0){
                    fprintf(stderr, "El w1 debe ser positivo.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optdeopso[0] = true;
                weighted = true;
                break;
            case 'Y':

                _w2 = atof(optarg);
                if(_w2 < 0.0){
                    fprintf(stderr, "El w2 debe ser positivo.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }
                weighted = true;
                optdeopso[1] = true;

                break;
            case 'Z':

                _w3 = atof(optarg);
                if(_w3 < 0.0){
                    fprintf(stderr, "El w3 debe ser positivo.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }
                weighted = true;
                optdeopso[2] = true;

                break;
            case 'd':
                break;
            default:
                fprintf(stderr, "Opción inválida.\nUse ./mhs --help para ayuda.\n");
                exit(1);
        }
    }

    if(!noerror){
        aux = true;
        fprintf(stderr, "Error en un(os) parámetro(s).\n");
    }

    for(c = 0; c < 4; c++)
        noerror = noerror && optgen[c];

    if(!noerror && !aux)
        fprintf(stderr, "Debe definir todas las opciones generales.\n");

    if(algorithm == M_KMEANS){

        //k
        if(!optextra[0])
            fprintf(stderr, "Debe definir todas las opciones del Kmeans.\n");

        noerror = noerror && optextra[0];

    }

    if(algorithm == M_ANT){

        //i
        if(!optindiv)
            fprintf(stderr, "Debe definir todas las opciones del Ant.\n");

        noerror = noerror && optindiv;

    }

    if(algorithm == M_BEE){

        aux = true;

        //e,eb,m y ob
        for(c = 0; c < 4; c++)
            aux = aux && optbee[c];

        //i y k
        aux = aux && optindiv && optextra[0];

        if(_I < (_e_bees + _o_bees)){
            fprintf(stderr, "La suma entre eb y ob debe ser menor a I.\n");
            aux = false;
        }

        if(_m_sites <= _e_sites){
            fprintf(stderr, "m debe ser mayor que e.\n");
            aux = false;
        }

        if(!aux)
            fprintf(stderr, "Debe definir todas las opciones del Bee.\n");

        noerror = noerror && aux;

    }

    if(algorithm == M_GA){

        aux = true;

        //pm y tt
        for(c = 0; c < 2; c++)
            aux = aux && optga[c];

        //i, pc y k
        aux = aux && optindiv && optdega && optextra[0];

        if(!aux)
            fprintf(stderr, "Debe definir todas las opciones del GA.\n");

        noerror = noerror && aux;

    }

    if(algorithm == M_DE){

        aux = true;

        //Si pc esta activada, igual f
        if( (!optdega && optde) || (!optde && optdega) ){
            aux = false;
            fprintf(stderr, "Debe definir f y pc (ambos), o simplemente no \
definirlos.\n");
        }

        //vectores mn y mx
        for(c = 0; c < 2; c++)
            aux = aux && optdepso[c];

        //pesos
        for(c = 0; c < 3; c++)
            aux = aux && optdeopso[c];

        if(aux){
            if((_w1 + _w2 + _w3) - 1.0 > 0.1){
                fprintf(stderr, "La suma de los pesos debe ser 1.0.\n");
                aux = false;
            }
        }

        //i y k
        aux = aux && optindiv && optextra[0];

        if(_I<4)
        {
            fprintf(stderr, "La cantidad de individuos debe ser mayor que 4.\n");
            aux = false;
        }

        if(!aux)
            fprintf(stderr, "Debe definir todas las opciones del DE.\n");

        noerror = noerror && aux;

    }

    if(algorithm == M_PSO){

        aux = true;

        //c1,c2,w y vmx
        for(c=0; c<4; c++)
            aux = aux && optpso[c];

        //vectores mn y mx
        for(c = 0; c < 2; c++)
            aux = aux && optdepso[c];

        //pesos
        if(weighted){
            for(c = 0; c < 3; c++)
                aux = aux && optdeopso[c];

            if(aux){
                if((_w1 + _w2 + _w3) - 1.0 > 0.1){
                    fprintf(stderr, "La suma de los pesos debe ser 1.0.\n");
                    aux = false;
                }
            }
        }

        //i y k
        aux = aux && optindiv && optextra[0];

        if(!aux)
            fprintf(stderr, "Debe definir todas las opciones del PSO.\n");

        noerror = noerror && aux;

    }

    if(noerror){
    
        r->read(_input);

        switch(algorithm){
            case M_KMEANS:
                m = new Kmeans(r->data, r->M, r->N, _K, M_DB, _reps);
                m->initialize();
                break;
            case M_GA:
                m = new GA(r->data, r->M, r->N, _K, _I, _pc, _pm, _tt, M_DB, _reps);
                break;
            case M_PSO:
                if(weighted){
                    m = new PSO(r->data, r->M, r->N, _K, _I,
                                _c1, _c2, _W,
                                _w1, _w2, _w3,
                                _mxv, _mnv, _vmx, F_WEIGHTED, _reps);
                }else{
                    m = new PSO(r->data, r->M, r->N, _K, _I,
                                _c1, _c2, _W,
                                _mxv, _mnv, _vmx, F_NON_WEIGHTED, _reps);
                }
                delete [] _mxv;
                delete [] _mnv;
                break;
            case M_DE:
                if(optde){
                    m = new DE(r->data, r->M, r->N, _K, _I, 
                                _reps, _pc, _f, _w1, _w2, _w3,
                                _mxv, _mnv);
                }else{
                    m = new DE(r->data, r->M, r->N, _K, _I,
                                _reps, _w1, _w2, _w3, _mxv, _mnv);
                }

                delete [] _mxv;
                delete [] _mnv;
                break;
            case M_ANT:
                if(optant){
                    m = new AntA(r->data, r->M, r->N, _I, _alpha, _reps, M_DB);
                }else{
                    m = new AntA(r->data, r->M, r->N, _I, _reps, M_DB);
                }
                break;
            default:
                m = new Bee(r->data, r->M, r->N, _K, _I, _m_sites, _e_sites, _e_bees, _o_bees, M_DB, _reps);
                break;
        }

    }else{
        fprintf(stderr, "Error en los argumentos, si tiene dudas use:\n\
./mhs --help\n");
        exit(-1);
    }

    delete [] optgen;
    delete [] optextra;
    delete [] optbee;
    delete [] optga;
    delete [] optpso;
    delete [] optdepso;
    delete [] optdeopso;

}

/**
 * Ejecuta la metaheurística elegida.
 */
void runIt(){
    srand(time(NULL));

    printf("-- Cantidad de Clusters Inicial: %d\n", m->K);

    initTime();

    initHandlers();

    isInHibrid = false;
    m->run(_tf);

    endTime();

    restoreHandlers();

    m->reconstruct(_tf);

    evalalg = m->ofEval;

    foalg = m->bestFO;

    algDB = m->calcDB();
    algJe = m->calcJe();

    
    if(algorithm != M_KMEANS){

        printf("-- Mejorando solución...\n");

        initTime();

        isInHibrid = true;
        evalhib = improve();

        endTime();

        fohib = m->bestFO;

        hibDB = m->calcDB();
        hibJe = m->calcJe();

    }

    printf("-- Cantidad de Clusters Final: %d\n", m->K);

    printf("** Algoritmo\n");

    printf("-- Cantidad de evaluaciones de la función objetivo del algoritmo: %d\n", evalalg);

    printf("-- Valor de la función objetivo del algoritmo: %.4f\n", foalg);

    printf("-- Índice DB del algoritmo: %.4f\n", algDB);

    printf("-- Error Je del algoritmo: %.4f\n", algJe);

    if(algorithm != M_KMEANS){

        printf("** Híbrido\n");

        printf("-- Cantidad de evaluaciones de la función objetivo del Kmeans: %d\n", evalhib);

        printf("-- Cantidad de evaluaciones de la función objetivo del híbrido: %d\n", (evalalg + evalhib));

        printf("-- Valor de la función objetivo del híbrido: %.4f\n", fohib);

        printf("-- Índice DB del híbrido: %.4f\n", hibDB);

        printf("-- Error Je del híbrido: %.4f\n", hibJe);
    }

    r->write(_output, m->bestSolution, m->K);
}

/**
 * Libera las variables reservadas.
 */
void cleanIt(){
    delete m;
    delete r;
}
