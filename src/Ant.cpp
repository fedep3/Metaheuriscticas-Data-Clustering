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
   
    free = true;
    pixel = -1;

}

/**
 * Destructor de la clase AntA.
 */
Ant::~Ant(){


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
 * Suelta el pixel que tiene.
 */
void Ant::drop(){

    pixel = -1;
    free = true;
    
}
