#include "utils.h"

#define OPT_GENERAL 0
#define OPT_GENETIC 1

#define RECOMMENDED_I   30
#define RECOMMENDED_CR  0.9
#define RECOMMENDED_MR  0.1
#define RECOMMENDED_TS  15

#define MAXRUNTIME 300

#define KMEANS  0
#define GENETIC 1

//General arguments.
int algorithm       = KMEANS;
int is_image        = 1;
int generateColors  = 0;

bool getParameters  = false;

int K                = 0;
string inputFile;
string outputFile;
int improvementLevel = 3;

//Genetic arguments.
int I = 0;
float crossoverRate = -1.0;
float mutationRate  = -1.0;
int tournamentSize  = 0;

//Time
struct timeval t_p;
double initialTime;
double runtime;

//Structures.
Reader* file;
BaseHeuristic* heuristic;

//Signal handlers.
struct sigaction new_sigint, old_sigint;
struct sigaction new_sigalrm, old_sigalrm;

//Function definitions.
void help(char*);
void checkArguments(char*);
void initStructures();
void initTime();
void initHandlers();
void killIt(int);
void restoreHandlers();
void endTime();
void showResults();

/**
 * Initializes the arguments for the algorithms.
 *
 * @param argc Quantity of arguments.
 * @param argv Arguments.
 */
void initIt(int argc, char* argv[]) {
    struct option general[] = {
        //Supported algorithms.
        {"kmeans",          no_argument, &algorithm,       KMEANS},
        {"genetic",         no_argument, &algorithm,      GENETIC},
        {"image",           no_argument, &is_image,             1},
        {"csv",             no_argument, &is_image,             0},
        {"generate-colors", no_argument, &generateColors,       1},

        //Required arguments.
        {"help",              no_argument,       0, 'h'},
        {"K",                 required_argument, 0, 'k'},
        {"input",             required_argument, 0, 'i'},
        {"output",            required_argument, 0, 'o'},
        {"improvement-level", required_argument, 0, 'l'},

        //Genetic algorithm options
        {"individuals",     required_argument, 0, 'I'},
        {"crossover-rate",  required_argument, 0, 'c'},
        {"mutation-rate",   required_argument, 0, 'm'},
        {"tournament-size", required_argument, 0, 't'}
    };

    int option_index = 0;
    char c;
    while( (c = getopt_long(argc, argv, "hk:i:o:l:I:c:m:t:01234", general, &option_index)) != -1) {

        switch(c) {
            case 0:
            case 1:
            case 2:
            case 3:
            case 4:
                break;
            case 'h':
                help(argv[0]);
                exit(0);
                break;
            case 'k':
                istringstream( optarg ) >> K;
                break;
            case 'i':
                inputFile = string(optarg);
                break;
            case 'o':
                outputFile = string(optarg);
                break;
            case 'l':
                istringstream( optarg ) >> improvementLevel;
                break;
            case 'I':
                istringstream( optarg ) >> I;
                break;
            case 'c':
                istringstream( optarg ) >> crossoverRate;
                break;
            case 'm':
                istringstream( optarg ) >> mutationRate;
                break;
            case 't':
                istringstream( optarg ) >> tournamentSize;
                break;
            default:
                printf("Wrong parameter: -%d\n", c);
                help(argv[0]);
                exit(1);
        }
    }

    checkArguments(argv[0]);

    initStructures();
}

/**
 * Checks arguments.
 */
void checkArguments(char* name) {
    if(K == 0 || inputFile.compare("") == 0 || outputFile.compare("") == 0) {
        fprintf(stderr, "Error required arguments not supplied.\n");
        help(name);
        exit(1);
    }

    if(algorithm == GENETIC) {
        if(I == 0 || I <= 1) {
            printf("Wrong value for the number of individuals: %d\n", I);
            printf("Set default: %d\n", RECOMMENDED_I);
            I = RECOMMENDED_I;
        }

        if(crossoverRate < 0.0 || crossoverRate > 1.1) {
            printf("Wrong value for the crossover rate: %.2f\n", crossoverRate);
            printf("Set default: %.2f\n", RECOMMENDED_CR);
            crossoverRate  = RECOMMENDED_CR;
        }

        if(mutationRate < 0.0 || mutationRate > 1.1) {
            printf("Wrong value for the mutation rate: %.2f\n", mutationRate);
            printf("Set default: %.2f\n", RECOMMENDED_MR);
            mutationRate   = RECOMMENDED_MR;
        }

        if(tournamentSize == 0 || tournamentSize > I) {
            printf("Wrong value for the tournament size: %d\n", RECOMMENDED_TS);
            printf("Set default: %d\n", RECOMMENDED_TS);
            tournamentSize = RECOMMENDED_TS;
        }
    }
}

/**
 * Initializes structures.
 */
void initStructures() {
    printf("** Initializing...\n");
    if(is_image)
        file = new ImageReader((bool) generateColors);
    else
        file = new Csv();

    (*file).read(inputFile.c_str());

    if(algorithm == KMEANS)
        heuristic = new Kmeans(&(file->data), K, improvementLevel);
    else
        heuristic = new Genetic(&(file->data), K, I, tournamentSize, crossoverRate, mutationRate, improvementLevel);
}

/**
 * Shows the help of the program.
 */
void help(char* name) {
    printf("Usage:\n");
    printf("%s -k <Int> -i <File> -o <File> [-l <Int>] [--image|--csv]\n\t[--kmeans", name);
    printf("|--genetic [-I <Int>] [-c <Float>] [-m <Float>] [-t <Int>]]\n\n");
    printf("Required options:\n");
    printf("\t-h   --help              Shows help.\n");
    printf("\t-k   --K                 Maximum number of clusters.\n");
    printf("\t-i   --input             Input file.\n");
    printf("\t-o   --output            Output file\n\n");
    printf("Optional arguments:\n");
    printf("\t-l   --improvement-level Quantity of iterations with no improvement\n");
    printf("\t                         of the solution (default = 3).\n");
    printf("\t--genetic                Chooses the genetic algorithm.\n");
    printf("\t--kmeans                 Chooses the kmeans algorithm (default).\n");
    printf("\t--csv                    Input file is a CSV file.\n");
    printf("\t--image                  Input file is an image (default).\n");
    printf("\t--generate-colors        Generates the output colors for better contrast.\n\n");
    printf("Genetic options:\n");
    printf("\t-I   --individuals       Number of individuals. (default = %d)   (> 1)\n", RECOMMENDED_I);
    printf("\t-c   --crossover-rate    Crossover rate value   (default = %.2f) (∈ [0.0, 1.0])\n", RECOMMENDED_CR);
    printf("\t-m   --mutation-rate     Mutation rate value    (default = %.2f) (∈ [0.0, 1.0])\n", RECOMMENDED_MR);
    printf("\t-t   --tournament-size   Tournament size.       (default = %d)   (≤ individuals)\n\n", RECOMMENDED_TS);
    printf("Examples:\n");
    printf("* Kmeans algorithm:\n");
    printf("\t%s --csv -i csv/iris.csv -o output -k 3\n", name);
    printf("\t%s -i img/lena.png -o output.png -k 32 --improvement-level 10\n\n", name);
    printf("* Genetic algorithm:\n");
    printf("\t%s --csv -i csv/iris.csv -o output -k 3 --generate-colors\n", name);
    printf("\t%s -i img/lena.png -o output.png -k 32 -I 30 -c 0.9 -m 0.1 -t 1\n\n", name);    
}

/**
 * Executes the chosen heuristic.
 */
void runIt() {
    printf("** Executing...\n");

    initTime();

    initHandlers();

    (*heuristic).run();

    endTime();

    restoreHandlers();

    showResults();
}

/**
 * Initialize counters.
 */
void initTime(){
    if (!gettimeofday(&t_p, NULL)){
        initialTime = (double) t_p.tv_sec + ((double) t_p.tv_usec)/1000000.0;
    } else {
        fprintf(stderr, "** Error when getting time.\n");
        exit(1);
    }
}

/**
 * Initializes the signal handlers.
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

    new_sigalrm.sa_handler = killIt;
    sigemptyset(&new_sigalrm.sa_mask);
    new_sigalrm.sa_flags = SA_NODEFER;
	sigaction(SIGALRM, &new_sigalrm, &old_sigalrm);

	alarm(MAXRUNTIME);
}

/**
 * Handles the termination signals.
 */
void killIt(int sig) {
    switch(sig) {
        case SIGALRM:
            printf("** Time exceded.\n");
            break;
        default:
            printf("** Terminated by user.\n");
    }

    endTime();

    restoreHandlers();

    showResults();

    cleanIt();

    exit(1);
}


/**
 * Restores the original handlers.
 */
void restoreHandlers() {
    sigemptyset(&old_sigint.sa_mask);
    old_sigint.sa_handler = SIG_DFL;
    old_sigint.sa_flags   = SA_NODEFER;
    sigaction(SIGINT, &old_sigint, NULL);

    sigemptyset(&old_sigalrm.sa_mask);
    new_sigalrm.sa_handler = SIG_DFL;
    new_sigalrm.sa_flags = SA_NODEFER;
    sigaction(SIGALRM, &old_sigalrm, NULL);
}

/**
 * Calculates runtime in seconds.
 */
void endTime() {
    if (!gettimeofday(&t_p,NULL)){
        runtime = (double) t_p.tv_sec + ((double) t_p.tv_usec)/1000000.0;
    }else{
        fprintf(stderr, "** Error when getting time.\n");
        exit(1);
    }

    runtime -= initialTime;
}

/**
 * Shows the results of the algorithm.
 */
void showResults() {
    printf("-- Initial number of clusters:              %d\n", K);
    printf("-- Solution's number of clusters:           %d\n", (*heuristic).bestIndividual.K);
    printf("-- Solution's Davies-Bouldin Index:         %.5f\n", (*heuristic).finalDB());
    printf("-- Number of DB index function evaluations: %d\n", (*heuristic).getNumberOfEvaluations());
    printf("-- Solution's Turi validity index:          %.5f\n", (*heuristic).finalTuri());
    printf("-- Execution time (in secs):                %1.4f\n", runtime);

    printf("** Generating image...\n");
    (*file).write( outputFile.c_str(),
                   (*heuristic).bestIndividual.solution,
                   (*heuristic).bestIndividual.centroid,
                   (*heuristic).bestIndividual.K);
}

/**
 * Frees memory.
 */
void cleanIt() {
    printf("** Cleaning...\n");
    delete heuristic;
    delete file;
}
