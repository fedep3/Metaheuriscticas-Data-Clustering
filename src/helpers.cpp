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

/**
 * Cantidad de repeticiones sin mejora.
 */
int _reps = 3;

///////////////////////////////////////////
// Opciones Generales.

/**
 * Tipo de función objetivo.
 */
int _tf = 0;
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
 * Cantidad (máxima) de clusters.
 */
int _K = 0;

/**
 * Tipo de metaheurística.
 */
int algorithm = -1;

/**
 * Metaheurística.
 */
Metaheuristic* m = NULL;

///////////////////////////////////////////
// Opciones Generales para poblacionales.

/**
 * Individuos o partículas.
 */
int _I = 0;

///////////////////////////////////////////
// Opciones del Algoritmo Genético.

/**
 * Probabilidad de mutación.
 */
float _pm = 0.0;

/**
 * Probabilidad de cruce.
 */
float _pc = 0.0;

/**
 * Tamaño del torneo.
 */
int _tt = 0;

///////////////////////////////////////////
// Opciones del Algoritmo PSO.

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

/**
 * Máximos valores de cada atributo.
 */
float* _mxv = NULL;

/**
 * Mínimos valores de cada atributo.
 */
float* _mnv = NULL;

/**
 * Velocidad máxima de las partículas.
 */
float _vmx = 0.0;

/**
 * Si la función es weighted o no.
 */
bool weighted = false;

///////////////////////////////////////////
// Opciones del Algoritmo DE.

/**
 * Parámetro de escalado de los vectores.
 */
float _f = 0.0;

/**
 * Si se usará la versión con máxima cantidad de clusters o no.
 */
bool _kmax = false;

/**
 * Inicializa el contador.
 */
void initTime();

/**
 * Guarda el tiempo del contador.
 */
void endTime();

/**
 * Hace varias cosas antes de terminar el programa:
 * - Muestra el tiempo.
 * - Genera una imagen con la mejor solución.
 * - Dice la cantidad de clusters.
 * @param sig Señal.
 */
void killIt(int sig){
    try{
        endTime();
        printf("Cantidad de Clusters Final: %d\n", m->K);
        r->write(_output, m->bestSolution, m->K);
        signal(SIGINT, SIG_DFL);
        exit(0);
    }catch(exception& e){
        cout << e.what() << endl;
        signal(SIGINT, SIG_DFL);
        exit(1);
    }
}

/**
 * Imprime la ayuda del programa.
 *
 * @param fullhelp Si es verdadero muestra la ayuda completa.
 */
void help(bool fullhelp){
    printf("Uso: \n\t./mhs <Opciones Generales> [--reps <Cantidad>]\
[<Opciones Poblacionales> <Opciones Del Algoritmo>]\n\n");

    if(fullhelp){
        printf("\t--reps <Cantidad>\n\
\t\t\033[34;1mAnt\033[0m   - Cantidad de iteraciones.\n\
\t\t\033[34;1mResto\033[0m - Cantidad de repeticiones sin mejora.\n");
        printf("\t--reps <Cantidad de repeticiones sin mejora>\n");
        printf("\t--help - Ayuda.\n");
        printf("\t-? - Ayuda.\n");
        //Opciones Generales.
        printf("\n\033[34;1mOpciones Generales\033[0m\n");
        //Tipo de Función Objetivo.
        printf("\t--tf <Tipo de Función Objetivo>\n\
\t\t\033[34;1mMAX\033[0m - Función de maximización.\n\
\t\t\033[34;1mMIN\033[0m - Función de minimización.\n");
        //Archivo de entrada.
        printf("\t--fi <Archivo de entrada>\n");
        //Archivo de salida.
        printf("\t--fo <Archivo de salida>\n");
        //Tipo de archivo.
        printf("\t--t  <Tipo de archivo de entrada>\n\
\t\t\033[34;1mPNG\033[0m  - Imagen PNG.\n\
\t\t\033[34;1mTIFF\033[0m - Imagen TIFF.\n\
\t\t\033[34;1mCSV\033[0m  - Archivo CSV.\n");
        //Cantidad de clusters.
        printf("\t--k <Cantidad de Clusters Iniciales>\n");
        //Algoritmo.
        printf("\t--a <Algoritmo a utilizar>\n\
\t\t\033[34;1mKmeans\033[0m - Algoritmo Kmeans.\n\
\t\t\033[34;1mGA\033[0m     - Algoritmo Genético..\n\
\t\t\033[34;1mPSO\033[0m    - Algoritmo PSO.\n\
\t\t\033[34;1mDE\033[0m     - Algoritmo DE.\n\
\t\t\033[34;1mAnt\033[0m    - Algoritmo Hormiga.\n\
\t\t\033[34;1mBee\033[0m    - Algoritmo Abeja.\n");


        //Opciones Poblacionales.
        printf("\n\033[34;1mOpciones Poblacionales\033[0m\n");
        //Cantidad de individuos/partículas.
        printf("\t--i <Cantidad de individuos/partículas>\n");


        //Opciones Algoritmo Genético.
        printf("\n\033[34;1mOpciones Algoritmo Genético\033[0m\n");
        //Probabilidad de mutación.
        printf("\t--pm <Probabilidad de mutación>\n");
        //Probabilidad de cruce.
        printf("\t--pc <Probabilidad de cruce>\n");
        //Tamaño del torneo.
        printf("\t--tt <Tamaño del torneo>\n");

        //Opciones Algoritmo PSO.
        printf("\n\033[34;1mOpciones Algoritmo PSO\033[0m\n");
        //Probabilidad de mutación.
        printf("\t--c1 <Constante de la componente cognitiva>\n");
        //Probabilidad de cruce.
        printf("\t--c2 <Constante de la componente social>\n");
        //Tamaño del torneo.
        printf("\t--W <Peso inercial>\n");
        //Peso de la distancia intracluster.
        printf("\t--w1 <Peso de la distancia intracluster>\n");
        //Peso de la distancia intercluster.
        printf("\t--w2 <Peso de la distancia intercluster>\n");
        //Peso del error.
        printf("\t--w3 <Peso del error>\n");
        //Valor mínimo de un atributo.
        printf("\t--mx <Valor máximo de un atributo>\n\
\t\t\033[34;1ma1,a2,...,am\033[0m - Donde ai es el máximo valor\n\
\t\t\tpara el atributo i.\n");
        //Valor máximo de un atributo.
        printf("\t--mn <Valor mínimo de un atributo>\n\
\t\t\033[34;1ma1,a2,...,am\033[0m - Donde ai es el mínimo valor\n\
\t\t\tpara el atributo i.\n");
        printf("\t--vmx <Velocidad máxima>\n");
        printf("\t--weighted - Si se utilizará la función objetivo\n\
\t\t\tcon pesos.\n");

        //Opciones Algoritmo DE.
        printf("\n\033[34;1mOpciones Algoritmo DE\033[0m\n");
        //Probabilidad de mutación.
        printf("\t--f <Parámetro de escalado>\n");
        //Probabilidad de cruce.
        printf("\t--pc <Probabilidad de cruce>\n");
        //Tamaño del torneo.
        printf("\t--kmax - Si se utilizará una cantidad máxima de clusters.\n");
    }
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

/**
 * Lee los argumentos del programa e inicializa las estructuras.
 *
 * @param argc Cantidad de Argumentos.
 * @param argv Argumentos.
 */
void initIt(int argc, char* argv[]){
    int c;
    int option_index;
    bool noerror = true;

    bool* optgen = new bool[6];
    for(c = 0; c < 6; ++c) optgen[c] = false;
    bool  indiv  = false;
    bool* optga  = new bool[3];
    for(c = 0; c < 3; ++c) optga[c]  = false;
    bool* optpso = new bool[9];
    for(c = 0; c < 9; ++c) optpso[c] = false;
    bool optde = false;

    //////////////////////////////////////////
    // Lectura de parámetros.
    // INICIO.

    while(true){
        static struct option long_options[] =
        {
            /* Ayuda */
            {"help", no_argument, 0, '?'},
            {"reps", required_argument, 0, '0'},
            /* Opciones generales */
            {"tf", required_argument, 0, 'A'},
            {"fi", required_argument, 0, 'B'},
            {"fo", required_argument, 0, 'C'},
            {"t",  required_argument, 0, 'D'},
            {"k",  required_argument, 0, 'E'},
            {"a",  required_argument, 0, 'F'},
            /* Opciones generales poblacionales */
            {"i",  required_argument, 0, 'G'},
            /* Opciones genético */
            {"pm", required_argument, 0, 'H'},
            {"pc", required_argument, 0, 'I'},
            {"tt", required_argument, 0, 'J'},
            /* Opciones PSO */
            {"c1", required_argument, 0, 'K'},
            {"c2", required_argument, 0, 'L'},
            {"W",  required_argument, 0, 'M'},
            {"w1", required_argument, 0, 'N'},
            {"w2", required_argument, 0, 'O'},
            {"w3", required_argument, 0, 'P'},
            {"mx", required_argument, 0, 'Q'},
            {"mn", required_argument, 0, 'R'},
            {"vmx", required_argument, 0,'S'},
            {"weighted", no_argument, 0, 'T'},
            /* Opciones del DE */
            {"kmax", no_argument, 0, 'U'}, //DE o DEKMax
            {"f", required_argument, 0,'V'}

            
        };

        option_index = 0;

        c = getopt_long(argc, argv,
                        "0:?A:B:C:D:E:F:G:H:I:J:K:L:M:N:O:P:Q:R:S:TUV:",
                        long_options, &option_index);

        if(c == -1)
            break;

        switch(c){
            case '0':
                _reps = atoi(optarg);
                if(_reps <= 0){
                    fprintf(stderr, "La cantidad de repeticiones debe ser mayor que 0.\n");
                    fprintf(stderr, "%s no lo es. Se utilizará 3 por defecto.\n", optarg);
                    _reps = 3;
                }
                break;
            case 'A':
                if(strcmp(optarg, "MAX") == 0){
                    _tf = T_MAX;
                }else if(strcmp(optarg, "MIN") == 0){
                    _tf = T_MIN;
                }else{
                    fprintf(stderr, "Debe elegir un tipo de función válido.\n");
                    fprintf(stderr, "MAX ó MIN son válidos, %s no lo es.\n", optarg);
                    noerror = false;
                }

                optgen[0] = true;
                break;
            case 'B':
                _input = optarg;
                if(r != NULL) r->read(_input);

                optgen[1] = true;
                break;
            case 'C':
                _output = optarg;

                optgen[2] = true;
                break;
            case 'D':
                if(strcmp(optarg, "PNG") == 0){
                    r = new Png();
                    if(_input != NULL) r->read(_input);
                }else if(strcmp(optarg, "TIFF") == 0){
                    r = new Tiff();
                    if(_input != NULL) r->read(_input);
                }else if(strcmp(optarg, "CSV") == 0){
                    r = new Csv();
                    if(_input != NULL) r->read(_input);
                }else{
                    fprintf(stderr, "Debe elegir un tipo de archivo válido.\n");
                    fprintf(stderr, "PNG, TIFF o CSV son válidos, %s no lo es.\n", optarg);
                    noerror = false;
                }

                optgen[3] = true;
                break;
            case 'E':
                _K = atoi(optarg);
                if(_K < 2){
                    fprintf(stderr, "La cantidad de clusters debe ser mayor que 2.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }

                optgen[4] = true;
                break;
            case 'F':
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
                    fprintf(stderr, "Kmeans, GA, PSO, DE, Ant o Bee son válidos, %s no lo es.\n", optarg);
                    noerror = false;
                }

                optgen[5] = true;
                break;
            case 'G':
                _I = atoi(optarg);
                if(_I <= 0){
                    fprintf(stderr, "La cantidad de individuos debe ser mayor que 0.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }
                indiv = true;
                break;
            //Genético.
            case 'H':
                _pm = atof(optarg);
                if(_pm < 0.0 && _pm > 1.0){
                    fprintf(stderr, "El porcentaje de mutación debe estar entre 0.0 y 1.0.\n");
                    fprintf(stderr, "%s no lo está.\n", optarg);
                    noerror = false;
                }
                optga[0] = true;
                break;
            case 'I':
                _pc = atof(optarg);
                if(_pc < 0.0 && _pc > 1.0){
                    fprintf(stderr, "El porcentaje de cruce debe estar entre 0.0 y 1.0.\n");
                    fprintf(stderr, "%s no lo está.\n", optarg);
                    noerror = false;
                }
                optga[1] = true;
                break;
            case 'J':
                _tt = atoi(optarg);
                if(_tt <= 0){
                    fprintf(stderr, "El tamaño del torneo debe ser mayor que 0.\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }
                optga[2] = true;
                break;
            //PSO
            case 'K':
                _c1 = atof(optarg);
                optpso[0] = true;
                break;
            case 'L':
                _c2 = atof(optarg);
                optpso[1] = true;
                break;
            case 'M':
                _W = atof(optarg);
                optpso[2] = true;
                break;
            case 'N':
                _w1 = atof(optarg);
                optpso[3] = true;
                break;
            case 'O':
                _w2 = atof(optarg);
                optpso[4] = true;
                break;
            case 'P':
                _w3 = atof(optarg);
                optpso[5] = true;
                break;
            case 'Q':
                parseVector(V_MAX);
                optpso[6] = true;
                break;
            case 'R':
                parseVector(V_MIN);
                optpso[7] = true;
                break;
            case 'S':
                _vmx = atof(optarg);
                if(_vmx <= 0.0){
                    fprintf(stderr, "La velocidad máxima debe ser mayor que 0.0\n");
                    fprintf(stderr, "%s no lo es.\n", optarg);
                    noerror = false;
                }
                optpso[8] = true;
                break;
            case 'T':
                weighted = true;
                break;
            case 'U':
                _kmax = true;
                break;
            case 'V':
                _f = atof(optarg);
                if(_f < 0.5 || _f > 1.0){
                    fprintf(stderr, "El parámetro de escalado f debe estar entre 0.5 y 1.0\n");
                    fprintf(stderr, "%s no lo está.\n", optarg);
                    noerror = false;
                }
                optde = true;
                break;
            case '?':
                help(true);
                exit(0);
                break;
            default:
                exit(1);
        }
    }

    // FIN

    //////////////////////////////////////////
    // Verificación de parámetros.
    // INICIO.

    //Opciones generales completas.
    switch(algorithm){
        case M_ANT:
            for(c = 0; c < 6; ++c)
                if(c != 4)
                    optgen[0] = optgen[0] && optgen[c];
            break;
        default:
            for(c = 0; c < 6; ++c) optgen[0] = optgen[0] && optgen[c];
    }

    if(!optgen[0]){
        fprintf(stderr, "Se deben definir todas las opciones generales.\n");
        if(algorithm == M_ANT)
            fprintf(stderr, "Excepto la cantidad de clusters.\n");
        noerror = false;
    }

    bool second = false;

    //Verificación por algoritmo.
    switch(algorithm){
        case M_KMEANS:
            if(!optgen[0])
                fprintf(stderr, "Se necesitan las opciones generales para ejecutar el Kmeans.\n");
            break;
        case M_GA:
            for(c = 0; c < 3; ++c) optga[0] = optga[0] && optga[c];
            if(!optga[0] || !indiv){
                fprintf(stderr, "Se necesitan definir todas las opciones del algoritmo genético\n");
                fprintf(stderr, "y la cantidad de individuos.\n");
                noerror = false;
            }
            break;
        case M_PSO:
            if(weighted)
                for(c = 0; c < 9; ++c) optpso[0] = optpso[0] && optpso[c];
            else{
                for(c = 0; c < 9; ++c){
                    switch(c){
                        case 3:
                        case 4:
                        case 5:
                            break;
                        default:
                            optpso[0] = optpso[0] && optpso[c];
                    }
                }
            }

            if(!optpso[0] || !indiv){
                fprintf(stderr, "Se necesitan definir todas las opciones del algoritmo PSO\n");
                fprintf(stderr, "y la cantidad de individuos.\n");
                noerror = false;
            }

            if(( ( (_c1 + _c2) * 0.5 ) - 1.0 ) >= _W){
                fprintf(stderr, "No se asegura convergencia con los valores de c1, c2 y W.\n");
                fprintf(stderr, "(c1 + c2) * 0.5 ) - 1  < W\n");
                fprintf(stderr, "(%.2f + %.2f) * 0.5 ) - 1 >= %.2f\n", _c1, _c2, _W);
                fprintf(stderr, "Se continuará la ejecución a pesar de esto.\n");
            }

            if(r->M != mxs || r->M != mns){
                fprintf(stderr, "Los tamaños de los vectores máximo o mínimos valores de atributos\n");
                fprintf(stderr, "no concuerdan\n");
                fprintf(stderr, "Cantidad de Atributos: %d\n", r->M);
                fprintf(stderr, "Cantidad de Atributos del Máximo: %d\n", mxs);
                fprintf(stderr, "Cantidad de Atributos del Mínimo: %d\n", mns);
                noerror = false;
            }
            break;
        case M_DE:
            if(_kmax){
                if(optde && optga[1])
                    second = true;
                algorithm = M_DEKMax;
            }else{
                if(!optpso[6] || !optpso[7]){
                    fprintf(stderr, "Debe definir los vectores de máximo y mínimo\n");
                    noerror = false;
                }

                if(r->M != mxs || r->M != mns){
                    fprintf(stderr, "Los tamaños de los vectores máximo o mínimos valores de atributos\n");
                    fprintf(stderr, "no concuerdan\n");
                    fprintf(stderr, "Cantidad de Atributos: %d\n", r->M);
                    fprintf(stderr, "Cantidad de Atributos del Máximo: %d\n", mxs);
                    fprintf(stderr, "Cantidad de Atributos del Mínimo: %d\n", mns);
                    noerror = false;
                }

                if(optde && optga[1])
                    second = true;
            }
            break;
        case M_ANT:
            printf("Se harán %d iteraciones.\n", _reps);
            if(!indiv){
                fprintf(stderr, "Debe definir la cantidad de hormigas.\n");
                noerror = false;
            }
            break;
        case M_BEE:
            break;
        default:
            fprintf(stderr, "Algoritmo desconocido.\n");
            exit(1);
    }

    // FIN

    //////////////////////////////////////////
    // Inicialiciación de algoritmos.
    // INICIO.

    if(noerror){
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
                if(second){
                    m = new DE(r->data, r->M, r->N, _K, _I,
                                _reps, _pc, _f,
                                _mxv, _mnv, 0);
                }else{
                    m = new DE(r->data, r->M, r->N, _K, _I,
                                _reps, _mxv, _mnv, 0);
                }

                delete [] _mxv;
                delete [] _mnv;
                break;
            case M_DEKMax:
                if(second){
                    m = new DEKMax(r->data, r->M, r->N, _K, _I, _reps, _pc, _f, M_DB);
                }else{
                    m = new DEKMax(r->data, r->M, r->N, _K, _I, _reps, M_DB);
                }
                break;
            case M_ANT:
                m = new AntA(r->data, r->M, r->N, _I, _reps, M_DB);
                break;
            case M_BEE:
                printf("Bee: Algoritmo no implementado.\n");
                exit(0);
                break;
            default:
                fprintf(stderr, "Algoritmo desconocido.\n");
                exit(1);
        }
    }else{
        help(false);
        exit(1);
    }

    // FIN

    delete [] optgen;
    delete [] optga;
    delete [] optpso;
}

/**
 * Inicializa el contador.
 */
void initTime(){


    if (!gettimeofday(&t_p, NULL)){
        tinit = (double) t_p.tv_sec +
                ((double) t_p.tv_usec)/1000000.0;
    }else{
        fprintf(stderr, "Problema con el contador de tiempo\n");
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
        fprintf(stderr, "Problema con el contador de tiempo\n");
        exit(1);
    }

    runtime = tend - tinit;
    printf("Tiempo de corrida: %1.4f segs.\n", runtime);
}

/**
 * Ejecuta la metaheurística elegida.
 */
void runIt(){
    srand(time(NULL));

    printf("Cantidad de Clusters Inicial: %d\n", m->K);

    initTime();

    signal(SIGINT, killIt);

    m->run(_tf);

    endTime();
    
    printf("Cantidad de Clusters Final: %d\n", m->K);

    r->write(_output, m->bestSolution, m->K);
}

/**
 * Libera las variables reservadas.
 */
void cleanIt(){
    delete m;
    delete r;
}
