/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase que representa una hormiga usada valga la redundancia en el algoritmo
 * de Hormiga.
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

    for(int i = 0; i < MM; i++)
        memory[i] = -1;

}

/**
 * Destructor de la clase AntA.
 */
Ant::~Ant(){

    delete [] memory; 

}

/*
 * Cambia el estado de la hormiga ya que tomó un pixel.
 * @param p Pixel que agarró.
 */
void Ant::pick(int p){

    pixel = p;
    free = false;
    
}

/*
 * Suelta el pixel que tiene y modifica su memoria agregando a ella
 * donde lo dejó.
 * @param cell Célula donde va a dejar el pixel.
 */
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

/*
 * Devuelve la posicion pos del arreglo de momoria.
 * @param pos Posición a acceder.
 */   
int Ant::getPixel(){

    return pixel;
    
}

/*
 * Devuelve el tamaño de la memoria.
 */
int Ant::getM(int pos){

    return memory[pos];
    
}

/*
 * Devuelve el tamaño de la memoria.
 */
int Ant::getMSize(){

    return msize;

}    

/*
 * Indica si la hormiga carga un pixel o no.
 */
bool Ant::isFree(){

    return free;
    
}

