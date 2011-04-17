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

    }catch(exception& e){
        cout << e.what() << endl;
    }

    return 0;
}
