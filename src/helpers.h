/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Archivo con funciones de ayuda y lectura de argumentos del
 * programa.
 */
#include <cstdio>
#include <cstdlib>
#include <getopt.h>
#include <cstring>
#include <vector>
#include <time.h>
#include <sys/time.h>
#include <signal.h>
#include "Reader.h"
#include "ImageReader.h"
#include "Png.h"
#include "Tiff.h"
#include "Csv.h"
#include "Metaheuristic.h"
#include "Kmeans.h"
#include "GA.h"
#include "PSO.h"
#include "AntA.h"
#include "DE.h"
#include "Bee.h"

#ifndef _HELPERS_
#define _HELPERS_

#define M_KMEANS 0
#define M_GA     1
#define M_PSO    2
#define M_DE     3
#define M_DEKMax 4
#define M_ANT    5
#define M_BEE    6

#define MAXRUNTIME  300

using namespace std;
///////////////////////////////////////////
// Tiempo.
extern double runtime;

///////////////////////////////////////////
// Opciones Generales.

/**
 * Tipo de función objetivo.
 */
extern int _tf;
/**
 * Nombre del archivo de entrada.
 */
extern char* _input;
/**
 * Nombre del archivo de salida.
 */
extern char* _output;

/**
 * Tipo de lector.
 */
extern Reader* r;

/**
 * Cantidad (máxima) de clusters.
 */
extern int _K;

/**
 * Tipo de metaheurística.
 */
extern int algorithm;

/**
 * Metaheurística.
 */
extern Metaheuristic* m;

///////////////////////////////////////////
// Opciones Generales para poblacionales.

/**
 * Individuos o partículas.
 */
extern int _I;

///////////////////////////////////////////
// Opciones del Algoritmo Genético.

/**
 * Probabilidad de mutación.
 */
extern double _pm;

/**
 * Probabilidad de cruce.
 */
extern double _pc;

/**
 * Tamaño del torneo.
 */
extern int _tt;

///////////////////////////////////////////
// Opciones del Algoritmo PSO.

/**
 * Constante de la componente cognitiva.
 */
extern double _c1;

/**
 * Constante de la componente social.
 */
extern double _c2;

/**
 * Peso inercial.
 */
extern double _W;

/**
 * Peso de la distancia intracluster.
 */
extern double _w1;

/**
 * Peso de la distancia intercluster;
 */
extern double _w2;

/**
 * Peso del error de la solución.
 */
extern double _w3;

/**
 * Máximos valores de cada atributo.
 */
extern double* _mxv;

/**
 * Mínimos valores de cada atributo.
 */
extern double* _mnv;

/**
 * Velocidad máxima de las partículas.
 */
extern double _vmx;

/**
 * Si la función es weighted o no.
 */
extern bool weighted;

/**
 * Imprime la ayuda del programa.
 *
 * @param fullhelp Si es verdadero muestra la ayuda completa.
 */
void help(bool fullhelp);

/**
 * Lee los argumentos del programa e inicializa las estructuras.
 *
 * @param argc Cantidad de Argumentos.
 * @param argv Argumentos.
 */
void initIt(int argc, char* argv[]);

/**
 * Ejecuta la metaheurística elegida.
 */
void runIt();

/**
 * Libera las variables reservadas.
 */
void cleanIt();
#endif
