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
float tinit = 0.0;

/**
 * Tiempo final.
 */
float tend = 0.0;

/**
 * Tiempo de ejecución.
 */
float runtime = 0.0;

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

///////////////////////////////////////////
// Opciones requeridas por varios algoritmos

/**
 * Cantidad (máxima) de clusters.
 */
int _K = 0;

/**
 * Tipo de función objetivo.
 */
int _tf = 0;
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


//////////////////////////////////////////////

using namespace boost;
namespace po = boost::program_options;

void initIt(int argc, char* argv[]){

    /** 
     * Ayuda
     */
    po::options_description help("Ayuda");
    help.add_options()
        ("help,?", "Produce mensaje de ayuda")
    ;

    /** 
     * Rquisitos genéricos
     */
    po::options_description generic("Requeridas genéricas");
    generic.add_options()
        ("a", po::value<string>(), "Algoritmo a ejecutar:\n\
Ant\n\
Bee\n\
DE\n\
GA\n\
Kmeans\n\
PSO")
        ("fi", po::value<string>(), "Archivo de entrada")
        ("fo", po::value<string>(), "Archivo de salida")
        ("t", po::value<string>(), "Tipo de archivo de entrada \
(CSV, TIFF o PNG)")
        ;

    /** 
     * Opciones requeridas por varios algorimos
     */
    po::options_description extra("Opciones requeridas por varios algorimos");
    extra.add_options()
        ("k", po::value<int>(&_K), "Número de clusters (Todos menos Ant)")
        ("reps", po::value<int>(&_reps)->default_value(3), "Número de iteracion en el\
 caso de Ant y DE, y cantidad de iteraciones que no se mejora en el caso de Bee\
, GA, Kmeans y PSO. El valor por default es 3")
        ("tf", po::value<string>()->default_value("MAX"), "Si se desea maximiza\
r o minimizar (MAX o MIN), el MAX está por defecto (Bee, GA y Kmeans)")
        ;

    /** 
     * Requerimiento poblacionales (Ant, Bee, DE, GA y PSO)
     */
    po::options_description pob("Requeridos poblacionales (Ant, Bee, DE, GA y P\
SO)");
    pob.add_options()
        ("i", po::value<int>(&_I), "Cantidad de individuos")
        ;

    /** 
     * Requeridos Bee
     */
    po::options_description bee("Requeridos Bee");
    bee.add_options()
        ("e", po::value<int>(&_e_sites), "Cantidad de parches élite")
        ("eb", po::value<int>(&_e_bees), "Cantidad de abejas a parches élite")
        ("m", po::value<int>(&_m_sites), "Cantidad de parches de exploración")
        ("ob", po::value<int>(&_o_bees), "Cantidad de abejas a parches no-élite")
        ;

    /** 
     * Opciones DE
     */
    po::options_description de("Opciones DE");
    de.add_options()
        ("f", po::value<float>(&_f), "Párametro escalar, requiere que el pc tambié\
n este establecido, sino se puede dejar de colocar ambos")
        ;

    /** 
     * Requeridos GA 
     */
    po::options_description ga("Requeridos GA");
    ga.add_options()
        ("pm", po::value<float>(&_pm), "Probabilidad de mutación")
        ("tt", po::value<int>(&_tt), "Tamaño del torneo")
        ;


    /** 
     * Requerido por DE y  opcional del GA
     */
    po::options_description dega("Requerido por DE y  opcional del GA");
    dega.add_options()
        ("pc", po::value<float>(&_pc), "Probabilidad de cruce, requerida en el gené\
tico, opcional en el DE (requiere el factor escalar esté establecido también)")
        ;

    /** 
     * Requeridos PSO
     */
    po::options_description pso("Requeridos PSO");
    pso.add_options()
        ("c1", po::value<float>(&_c1), "Constante de la componente cognitiva")
        ("c2", po::value<float>(&_c2), "Constante de la componente social")
        ("w", po::value<float>(&_W), "Peso inercial")
        ("vmx", po::value<float>(&_vmx), "Velocidad máxima")
    ;


    /** 
     * Opcional PSO
     */
    po::options_description opso("Opcional PSO");
    opso.add_options()
        ("weighted", "Indica si se van a usar los pesos o no en el algoritmo")
    ;

    /** 
     * Requeridos DE y PSO
     */
    po::options_description depso("Requeridos DE y PSO");
    depso.add_options()
        ("mn", po::value<string>(), "Vector de valores mínimos de cada atribut\
o")
        ("mx", po::value<string>(), "Vector de valores máximos de cada atribut\
o")
    ;

    /** 
     * Requeridos DE y opcional PSO
     */
    po::options_description deopso("Requeridos DE y opcional PSO");
    deopso.add_options()
        ("w1", po::value<float>(&_w1), "Peso de la distancia intracluster")
        ("w2", po::value<float>(&_w2), "Peso de la distancia intercluster")
        ("w3", po::value<float>(&_w3), "Peso del error.\
La suma debe ser w1+w2+w3 = 1.0")
        ;

    po::options_description visible("Opciones permitidas");
    visible.add(help).add(generic).add(extra).add(pob).add(bee).add(de);
    visible.add(ga).add(dega).add(pso).add(opso).add(depso).add(deopso);

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, visible), vm);
    po::notify(vm);

    int c;  

    bool* optgen = new bool[4];
    for(c = 0; c < 4; ++c) optgen[c] = false;
    bool* optextra = new bool[3];
    for(c = 0; c < 3; ++c) optextra[c] = false;
    bool  indiv  = false;
    bool* optbee  = new bool[4];
    for(c = 0; c < 4; ++c) optbee[c] = false;
    bool optde = false;
    bool* optga  = new bool[2];
    for(c = 0; c < 2; ++c) optga[c] = false;
    bool optdega = false;
    bool* optpso  = new bool[4];
    for(c = 0; c < 4; ++c) optpso[c] = false;
    bool wieghted = false;
    bool* optdepso  = new bool[2];
    for(c = 0; c < 2; ++c) optdepso[c] = false;
    bool* optdeopso  = new bool[3];
    for(c = 0; c < 3; ++c) optdeopso[c] = false;

    if (vm.count("help")) {
        cout << "Uso: \n\t./mhs <Requeridas Genéricas> \
<Opciones Del Algoritmo>]\n\n"<< visible << "\n";
        return;
    }

}
