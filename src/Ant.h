/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase concreta donde se definen las funciones y variables
 * necesarias para ejecutar el diferential evolution.
 */

#ifndef _Ant_
#define _Ant_

#include <cstdio>
#include <time.h>
#include <vector>
#include "Metaheuristic.h"

using namespace std;

#define MM 5


using namespace std;

class Ant{
    public:

        Ant();

        ~Ant();

        void pick(int p);

        void drop(int cell);
        
        int getPixel();
        
        int getM(int pos);
        
        int getMSize();

        bool isFree();

    private:

        int pixel;

        bool free;

        int *memory;

        int msize;


};
#endif
