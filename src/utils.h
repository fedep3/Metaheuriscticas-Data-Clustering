#include <getopt.h>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <sstream>
#include <ctime>
#include <sys/time.h>
#include <signal.h>
#include <unistd.h>
#include "Data.h"
#include "ImageReader.h"
#include "Csv.h"
#include "Kmeans.h"
#include "Genetic.h"

using namespace std;

/**
 * Initializes the arguments for the algorithms.
 *
 * @param argc Quantity of arguments.
 * @param argv Arguments.
 */
void initIt(int argc, char* argv[]);

/**
 * Executes the chosen heuristic.
 */
void runIt();

/**
 * Frees memory.
 */
void cleanIt();
