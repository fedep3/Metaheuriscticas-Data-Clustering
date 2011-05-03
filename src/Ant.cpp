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
    count = new int[MM];
    nit = 0;

    for(int i = 0; i < MM; i++){
        count[i] = 0;
        memory[i] = -1;
    }

}

/**
 * Destructor de la clase AntA.
 */
Ant::~Ant(){

    delete [] memory; 
    delete [] count;

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
    int aux = 0, i = 0;

    pixel = -1;
    free = true;
    nit++;

    for(i = 0; i < MM; i++){
        if(memory[i] == cell){
            done = true;
            count[i]++;
            break;
        }
    }
    
    if(!done){

        if(msize < MM){
        
            memory[msize] = cell;
            count[msize]++;
            msize++;
            
        }
    }

    if(nit == 10){

        int min = 20, mini = 0;

        for(i = 0; i < MM; i++){
            if( (aux = count[i]) < min){
                min = aux;
                mini = i;
            }
        }

        for( i = mini; i < MM-1; i++)
            memory[i] = memory[i+1];

        msize--;
        nit = 0;

        for(i = 0; i < MM; i++)
            count[i] = 0;

    }
    
}
