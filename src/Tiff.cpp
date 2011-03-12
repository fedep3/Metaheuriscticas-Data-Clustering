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
#include "Tiff.h"

/**
 * Constructor de la clase Csv.
 */
Tiff::Tiff()
: ImageReader(){}

/**
 * Destructor de la clase Csv.
 */
Tiff::~Tiff(){
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
void Tiff::read(char* input){
    int i, w, h;
    TIFF *image;
    uint32 *raster, _width, _height;
    uint16 depth;

    if((image = TIFFOpen(input, "r")) == NULL){
        fprintf(stderr, "No se pudo abrir la imagen.\n");
        exit(1);
    }

    TIFFGetField(image, TIFFTAG_IMAGEWIDTH, &_width);
    TIFFGetField(image, TIFFTAG_IMAGELENGTH, &_height);
    TIFFGetField(image, TIFFTAG_SAMPLESPERPIXEL, &depth);

    height = _height;
    width  = _width;
    N = height * width;
    M = (int) depth;

    if( (int)depth != 1 && (int)depth != 3){
        fprintf(stderr, "Tipo de imagen no reconocido.\n");
        TIFFClose(image);
        exit(1);  
    }

    if((raster = (uint32*) _TIFFmalloc(N * sizeof (uint32)) ) == NULL){
        fprintf(stderr, "Error pidiendo memoria.\n");
        TIFFClose(image);
        exit(1);
    }

    if((data = (float **) calloc(N, sizeof(float *))) == NULL){
        fprintf(stderr, "Error pidiendo memoria.\n");
        TIFFClose(image);
        exit(1);
    }

	if(TIFFReadRGBAImageOriented(image, _width, _height, raster, ORIENTATION_TOPLEFT, 0)){
        switch(depth){
            case 3:
                for(i = 0; i < N; ++i)
                    data[i] = new float[3];

                i = 0;

                for(h = 0; h < height; ++h){
                    for(w = 0; w < width; ++w){
                        data[i][0] = (float) TIFFGetR(raster[h * width + w]);
                        data[i][1] = (float) TIFFGetG(raster[h * width + w]);
                        data[i][2] = (float) TIFFGetB(raster[h * width + w]);
                        ++i;
                    }
                }
                break;
            case 1:
                for(i = 0; i < N; ++i)
                    data[i] = new float[1];

                i = 0;

                for(h = 0; h < height; ++h){
                    for(w = 0; w < width; ++w){
                        data[i][0] = (float) TIFFGetR(raster[h * width + w]);
                        ++i;
                    }
                }
                break;
            default:
                fprintf(stderr, "Profundidad errónea.\n");
                TIFFClose(image);
                exit(1);
        }
    }
	_TIFFfree(raster);

    TIFFClose(image);
}
