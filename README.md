#Atenea

Part of undergrad thesis done by Federico Ponte and Alexander De Sousa. We were both Computer Engineering students from Universidad Simón Bolívar in Venezuela.

This is a clean code for the Genetic Algorithm and Kmeans with a better image I/O library.

You can download the book [here](https://www.dropbox.com/s/gvek7s4rbwvraxe/thesis.pdf?dl=0), it is in Spanish.

##Required packages

    - libmagick++-dev (On Debian)

##Usage

```bash
./athena -k <Int> -i <File> -o <File> [-l <Int>] [--image|--csv]
	[--kmeans|--genetic [-I <Int>] [-c <Float>] [-m <Float>] [-t <Int>]]
```

###Required options 
	-h   --help              Shows help.
	-k   --K                 Maximum number of clusters.
	-i   --input             Input file.
	-o   --output            Output file

###Optional arguments:
	-l   --improvement-level Quantity of iterations with no improvement
	                         of the solution (default = 3).
	--genetic                Chooses the genetic algorithm.
	--kmeans                 Chooses the kmeans algorithm (default).
	--csv                    Input file is a CSV file.
	--image                  Input file is an image (default).
	--generate-colors        Generates the output colors for better contrast.

###Genetic options:
	-I   --individuals       Number of individuals. (default = 30)   (> 1)
	-c   --crossover-rate    Crossover rate value   (default = 0.90) (∈ [0.0, 1.0])
	-m   --mutation-rate     Mutation rate value    (default = 0.10) (∈ [0.0, 1.0])
	-t   --tournament-size   Tournament size.       (default = 15)   (≤ individuals)

###Examples:
* Kmeans algorithm:

	- ./athena --csv -i csv/iris.csv -o output -k 3
	- ./athena -i img/lena.png -o output.png -k 32 --improvement-level 10 --generate-colors

* Genetic algorithm:
	- ./athena --csv -i csv/iris.csv -o output -k 3
	- ./athena -i img/lena.png -o output.png -k 32 -I 30 -c 0.9 -m 0.1 -t 1

