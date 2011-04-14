/**
 * @file
 *
 * @author Alexander De Sousa 06-39439, 
 *         Federico Ponte     06-40108
 *
 * @section Descripción
 *
 * Clase concreta en donde se definen las funciones básicas de
 * lectura de CSV.
 */
#include "Png.h"

/**
 * Constructor de la clase Csv.
 */
Png::Png()
: ImageReader(){}

/**
 * Destructor de la clase Csv.
 */
Png::~Png(){
    int i;
    for(i = 0; i < N; ++i)
        delete [] data[i];
    free(data);
}

/**
 * Lee el archivo y arma las estructuras de datos
 * necesarias.
 *
 * @param input Archivo de entrada.
 */
void Png::read(char* input){
    int i, j;
    vector<unsigned char> buffer, image;

    LodePNG::loadFile(buffer, input);
    LodePNG::Decoder decoder; 
    decoder.inspect(buffer);
    if(!decoder.hasError()){
        if(decoder.isGreyscaleType()) M = 1;
        else                          M = 3;
    }else{
        printf("Error al leer la imagen.\n");
        exit(1);
    }

    switch(M){
        case 3:
            decoder.decode(image, buffer.empty() ? 0 : &buffer[0], (unsigned)buffer.size());

            if (decoder.getChannels() < 3 || decoder.getBpp() < 24) {
                fprintf(stderr,"La imagen debe ser de 8 bits por canal.\n");
                exit(1);
            }

            width  = decoder.getWidth();
            height = decoder.getHeight();
            N = width * height;
            break;
        case 1:
            decoder.decode(image, buffer.empty() ? 0 : &buffer[0], (unsigned)buffer.size());

            width  = decoder.getWidth();
            height = decoder.getHeight();
            N = width * height;
    }

    if((data = (double **) calloc(N, sizeof(double *))) == NULL){
        fprintf(stderr, "Error pidiendo memoria.\n");
        exit(1);
    }
    for(i = 0; i < N; ++i)
        data[i] = new double[M];

    vector<unsigned char>::iterator imageIterator = image.begin();

    switch(M){
        case 3:
            i = 0;

            while (imageIterator != image.end()) {
                for(j = 0; j < 3; ++j){
                    data[i][j] = (double) (*imageIterator);
                    imageIterator++;
                }
                imageIterator++; 
                ++i;
            }
            break;
        case 1:
            while (imageIterator != image.end()) {
                data[i][0] = (*imageIterator);
                imageIterator++;
                ++i;
            }
    }
}
