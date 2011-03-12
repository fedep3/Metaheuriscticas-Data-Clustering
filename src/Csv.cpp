/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase concreta para lectura de CSVs.
 */

#include "Csv.h"

/**
 * Constructor de la clase Reader.
 */
Csv::Csv(){ }

/**
 * Destructor de la clase Reader.
 */
Csv::~Csv(){ }

/**
 * Lee el archivo y arma las estructuras de datos
 * necesarias.
 *
 * @param input Archivo de entrada.
 */
void Csv::read(char* input){
    int I = 0;
    int J = 0;
    int i, j, k;
    bool first = true;
    ifstream lines(input);
    vector<float> v;

    string line;
    while(getline(lines,line)){
        if(strcmp(line.c_str(), "") == 0) continue;
        stringstream  lineStream(line);
        string cell;

        j = 0;
        while(getline(lineStream, cell, ',')){
            if(first) ++J;
            v.push_back( atof( cell.c_str()) );
            ++j;
        }

        if(!first){
            if(J != j){
                fprintf(stderr, "Error en la línea %d de %s.\n", (I + 1), input);
                fprintf(stderr, "Cantidad de atributos no concuerda.\n");
                exit(1);
            }
        }
        first = false;
        ++I;
    }

    N = I;
    M = J;

    lines.close();

    data = (float**) calloc(I, sizeof(float*));
    if(data == NULL){
        fprintf(stderr, "No se pudo reservar memoria.\n");
        exit(1);
    }
    k = 0;
    for(i = 0; i < I; ++i){
        data[i] = new float[J];
        for(j = 0; j < J; ++j){
            data[i][j] = v[k];
            ++k;
        }
    }
}

/**
 * Escribe los resultados en el archivo dado.
 *
 * @param output Archivo de salida.
 * @param sol    Solución final.
 * @param k      Cantidad de clusters.
 */
void Csv::write(char* output, int* sol, int k){
    int i, j, l;
    ofstream lines(output);

    for(l = 0; l < k; ++l){
        lines << "Cluster " << l << endl;
        for(i = 0; i < N; ++i){
            if(sol[i] == l){
                j = 0;
                lines << "Objeto " << i << ":\t" << data[i][j]; 
                for(j = 1; j < M; ++j){
                    lines << ", " << data[i][j];
                }
                lines << endl;
            }
        }
        lines << endl;
    }

    lines.close();
}
