/*
Copyright (c) 2011 Alexander De Sousa, Federico Ponte

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
Software), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
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
