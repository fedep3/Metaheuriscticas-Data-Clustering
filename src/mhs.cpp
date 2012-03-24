/*
Data Clustering Metaheuristics focused on images.
Copyright (C) 2011 Alexander De Sousa(prof.etadel2@gmail.com), 
                                                Federico Ponte(fedep3@gmail.com)

This program is free software; you can redistribute it and/or modify it under 
the terms of the GNU General Public License as published by the Free Software 
Foundation; either version 2 of the License, or (at your option) any later 
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY 
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
PARTICULAR PURPOSE. See the GNU General Public License for more details.
*/
#include "helpers.h"

/**
 * Programa principal de mhs.
 *
 * @param argc Cantidad de argumentos.
 * @param argv Argumentos.
 */
int main(int argc, char* argv[]){
    try{
        //Inicializa el algoritmo.
        initIt(argc, argv);

        //Ejecuta y escribe los resultados del algoritmo.
        runIt();

        //Libera las variables.
        cleanIt();

    }catch(exception& e){
        cout << e.what() << endl;
    }

    return 0;
}
