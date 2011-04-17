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
// Requeridos Ant

/**
 * Promedio de diferencia entre todos los posibles par de vector de atributos.
 */
float _alpha;

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

#define V_MAX 0
int mxs = 0;
#define V_MIN 1
int mns = 0;
/**
 * Parsea el vector de acuerdo a un string.
 */
void parseVector(string vec, int t){
    int i;
    int vs;
    vector<float> v;
    float* temp;
    stringstream line(vec);
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
        ("reps", po::value<int>(&_reps)->default_value(3), "Número de iteracion\
 en el caso de Ant y DE, y cantidad de iteraciones que no se mejora en el caso \
de Bee, GA, Kmeans y PSO. El valor por default es 3")
        ("tf", po::value<string>()->default_value("MAX"), "Si se desea maximiza\
r o minimizar (MAX o MIN), el MAX está por defecto (Bee, GA y Kmeans)")
        ;

    /** 
     * Requerimiento poblacionales (Ant, Bee, DE, GA y PSO)
     */
    po::options_description ant("Opcional Ant");
    ant.add_options()
        ("alpha", po::value<float>(&_alpha), "Promedio de diferencia entre todos \
los posibles par de vector de atributos")
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
        ("ob", po::value<int>(&_o_bees), "Cantidad de abejas a parches \
no-élite")
        ;

    /** 
     * Opciones DE
     */
    po::options_description de("Opciones DE");
    de.add_options()
        ("f", po::value<float>(&_f), "Párametro escalar, requiere que el pc tam\
bién este establecido, sino se puede dejar de colocar ambos")
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
     * Requerido por GA y opcional DE
     */
    po::options_description dega("Requerido por GA y opcional DE");
    dega.add_options()
        ("pc", po::value<float>(&_pc), "Probabilidad de cruce, requerida en el \
genético, opcional en el DE (requiere el factor escalar esté establecido tambié\
n)")
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
    visible.add(help).add(generic).add(extra).add(pob).add(ant).add(bee);
    visible.add(de).add(ga).add(dega).add(pso).add(opso).add(depso).add(deopso);

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, visible), vm);
    po::notify(vm);

    int c;  

    bool* optgen = new bool[4];
    for(c = 0; c < 4; ++c) optgen[c] = false;
    bool* optextra = new bool[3];
    for(c = 0; c < 3; ++c) optextra[c] = false;
    bool  optindiv  = false;
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
    bool noerror = true;
    bool aux;

    if (vm.count("help")) {
        cout << "Uso: \n\t./mhs [-?|--help] <Requeridas Genéricas> \
<Opciones Del Algoritmo>]\n\n"<< visible << "\n";
        exit(1);
    }

    if (vm.count("a")) {

        if(vm["a"].as<string>().compare("Kmeans") == 0){
            algorithm = M_KMEANS;
        }else if(vm["a"].as<string>().compare("GA") == 0){
            algorithm = M_GA;
        }else if(vm["a"].as<string>().compare("PSO") == 0){
            algorithm = M_PSO;
        }else if(vm["a"].as<string>().compare("DE") == 0){
            algorithm = M_DE;
        }else if(vm["a"].as<string>().compare("Ant") == 0){
            algorithm = M_ANT;
        }else if(vm["a"].as<string>().compare("Bee") == 0){
            algorithm = M_BEE;
        }else{
            cerr << "Debe elegir un algoritmo válido.\nKmeans, GA, PSO, \
DE, Ant o Bee son válidos, " << vm["a"].as<string>() << " no lo es \n";
            noerror = false;
        }

        optgen[0] = true;
    }

    if (vm.count("fi")) {

        _input = (char *) vm["fi"].as<string>().c_str();
        optgen[1] = true;

    }

    if (vm.count("fo")) {

        _output = (char *)vm["fo"].as<string>().c_str();
        optgen[2] = true;

    }

    if (vm.count("t")) {

        if(vm["t"].as<string>().compare("PNG") == 0){
            r = new Png();
        }else if(vm["t"].as<string>().compare("TIFF") == 0){
            r = new Tiff();
        }else if(vm["t"].as<string>().compare("CSV") == 0){
            r = new Csv();
        }else{
            cerr << "Debe elegir un tipo de archivo válido.\n PNG, TIFF o CSV \
son válidos, " << vm["t"].as<string>() << " no lo es.\n", 
            noerror = false;
        }

        r->read(_input);

        optgen[3] = true;

    }

    for(c = 0; c < 4; c++)
        noerror = noerror && optgen[c];

    if(!noerror)
        cerr << "Debe definir todas las opciones generales\n";

    if (vm.count("k")) 
        optextra[0] = true;

    if (vm.count("reps")) 
        optextra[1] = true;

    if (vm.count("tf")){

        if(vm["tf"].as<string>().compare("MAX") == 0){
            _tf = T_MAX;
        }else if(vm["tf"].as<string>().compare("MIN") == 0){
            _tf = T_MIN;
        }else{
            cerr << "Debe elegir un tipo de función válido.\n MAX ó MIN son vál\
idos, " << vm["tf"].as<string>() << " no lo es.\n";
            noerror = false;
        }

        optextra[2] = true;

    }

    if(algorithm == M_KMEANS){

        if(!optextra[0])
            cerr << "Debe definir todas las opciones del Kmeans\n";

        noerror = noerror && optextra[0];

    }

    if(algorithm == M_ANT){

        if(!optindiv)
            cerr << "Debe definir todas las opciones del Ant\n";

        noerror = noerror && optindiv;

    }

    if (vm.count("i")) 
        optindiv = true;

    if (vm.count("e")) 
        optbee[0] = true;

    if (vm.count("eb")) 
        optbee[1] = true;

    if (vm.count("m")) 
        optbee[2] = true;

    if (vm.count("ob")) 
        optbee[3] = true;

    if(algorithm == M_BEE){

        aux = true;

        for(c = 0; c < 4; c++)
            aux = aux && optbee[c];

        aux = aux && optindiv && optextra[0];

        if(!aux)
            cerr << "Debe definir todas las opciones del Bee\n";

        noerror = noerror && aux;

    }

    if (vm.count("f")) 
        optde = true;

    if (vm.count("pm")) 
        optga[0] = true;

    if (vm.count("tt")) 
        optga[1] = true;

    if (vm.count("pc")) 
        optdega = true;

    if(algorithm == M_GA){

        aux = true;

        for(c = 0; c < 2; c++)
            aux = aux && optga[c];

        aux = aux && optindiv && optdega && optextra[0];

        if(!aux)
            cerr << "Debe definir todas las opciones del GA\n";

        noerror = noerror && aux;

    }

    if (vm.count("c1")) 
         optpso[0] = true;

    if (vm.count("c2")) 
         optpso[1] = true;

    if (vm.count("w")) 
         optpso[2] = true;

    if (vm.count("vmx")) 
         optpso[3] = true;

    if(vm.count("wieghted"))
        wieghted = true;

    if(vm.count("mn")){

        optdepso[0] = true;
        parseVector(vm["mn"].as<string>(), V_MIN);

    }

    if(vm.count("mx")){

        optdepso[1] = true;
        parseVector(vm["mx"].as<string>(), V_MIN);

    }

    if(vm.count("w1"))
        optdeopso[0] = true;

    if(vm.count("w2"))
        optdeopso[1] = true;

    if(vm.count("w3"))
        optdeopso[2] = true;




}
