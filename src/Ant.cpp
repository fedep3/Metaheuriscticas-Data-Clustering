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

#include "Ant.h"

/**
 * Constructor de la clase Ant.
 */
Ant::Ant(){
   
    memory = new int[MM];
    msize = 0;
    free = true;
    pixel = -1;

}

/**
 * Destructor de la clase AntA.
 */
Ant::~Ant(){

    delete [] memory; 

}

void Ant::pick(int p){

    pixel = p;
    free = false;
    
}

void Ant::drop(int cell){

    bool done = false;
    int aux = 0, ant = 0, i = 0;

    pixel = -1;
    free = true;

    for(i = 0; i < MM; i++){
        if(memory[i] == cell){
            done = true;
            break;
        }
    }
    
    if(!done){

        if(msize < MM){
        
            memory[msize] = cell;
            msize++;
            
        }else{
        
            aux = memory[0];
            
            for(i = 1; i < msize; i++){
                ant = aux;
                aux = memory[i];
                memory[i] = ant;
            }
                
            memory[0] = cell;
            
        }

    }
}

int Ant::getPixel(){

    return pixel;
    
}

int Ant::getM(int pos){

    return memory[pos];
    
}

int Ant::getMSize(){

    return msize;

}    

bool Ant::isFree(){

    return free;
    
}

