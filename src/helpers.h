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

#include <getopt.h>
#include <cstdio>
#include <cstdlib>
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

#define FERROR 0.05

#define MAXRUNTIME  300

using namespace std;
///////////////////////////////////////////
// Tiempo.

/**
 * Tiempo de ejecución.
 */
extern double runtime;

///////////////////////////////////////////
// Rquisitos genéricos

/**
 * Tipo de metaheurística.
 */
extern int algorithm;
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
 * Metaheurística.
 */
extern Metaheuristic* m;

///////////////////////////////////////////
// Opciones requeridas por varios algoritmos

/**
 * Cantidad (máxima) de clusters.
 */
extern int _K;

/**
 * Tipo de función objetivo.
 */
extern int _tf;
/**
 * Cantidad de repeticiones sin mejora.
 */
extern int _reps;

///////////////////////////////////////////
// Requerimiento poblacionales (Ant, Bee, DE, GA y PSO)

/**
 * Individuos o partículas.
 */
extern int _I;

///////////////////////////////////////////
// Requeridos Ant

/**
 * Promedio de diferencia entre todos los posibles par de vector de atributos.
 */
extern float _alpha;

///////////////////////////////////////////
// Requeridos Bee

/**
 * Cantidad de sitios a explorar.
 */
extern int _m_sites;

/**
 * Cantidad de sitios élite.
 */
extern int _e_sites;

/**
 * Cantidad de abejas a sitios élite.
 */
extern int _e_bees;

/**
 * Cantidad de abejas a sitios no-élite.
 */
extern int _o_bees;

///////////////////////////////////////////
// Opciones DE

/**
 * Parámetro de escalado de los vectores.
 */
extern float _f;

///////////////////////////////////////////
// Requeridos GA 

/**
 * Probabilidad de mutación.
 */
extern float _pm;
/**
 * Tamaño del torneo.
 */
extern int _tt;


///////////////////////////////////////////
// Requerido por DE y  opcional del GA

/**
 * Probabilidad de cruce.
 */
extern float _pc;

///////////////////////////////////////////
// Requeridos PSO

/**
 * Constante de la componente cognitiva.
 */
extern float _c1;

/**
 * Constante de la componente social.
 */
extern float _c2;

/**
 * Peso inercial.
 */
extern float _W;
/**
 * Velocidad máxima de las partículas.
 */
extern float _vmx;


///////////////////////////////////////////
// Opcional PSO

/**
 * Si la función es weighted o no.
 */
extern bool weighted;

///////////////////////////////////////////
// Requeridos DE y PSO

/**
 * Máximos valores de cada atributo.
 */
extern float* _mxv;

/**
 * Mínimos valores de cada atributo.
 */
extern float* _mnv;

///////////////////////////////////////////
// Requeridos DE y opcional PSO

/**
 * Peso de la distancia intracluster.
 */
extern float _w1;

/**
 * Peso de la distancia intercluster;
 */
extern float _w2;

/**
 * Peso del error de la solución.
 */
extern float _w3;

///////////////////////////////////////////

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
