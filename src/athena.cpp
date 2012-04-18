#include <cstdio>
#include <cstdlib>
#include "utils.h"
#include "Kmeans.h"
#include "Genetic.h"
#include "ImageReader.h"

int main(int argc, char* argv[]) {
    initIt(argc, argv);

    runIt();

    cleanIt();

    return 0;
}
