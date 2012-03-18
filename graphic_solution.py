#!/usr/bin/python
# -*- coding: utf-8 -*-
"""
Para correrlo asegurate de:
sudo aptitude install python-matplotlib python-numpy python-scipy
"""

import numpy
import re
from sys import argv
from scipy import cluster
from matplotlib import pyplot

def print_help():
    print "Se debe usar de la siguiente manera:\n./graphic_solution archivo_csv dimension1 dimension2"
    exit(-1)

if __name__ == "__main__":

    if len(argv) != 4:
        print_help()

    csv_file = open(argv[1], 'r')
    dimensions_taken_into_account = [int(dimension_number) for dimension_number in argv[2:]]

    if len(dimensions_taken_into_account) != 2:
        print_help()

    cluster_re = re.compile('Cluster \d+')
    object_re = re.compile('(Objeto \d+:\t)([a-zA-Z0-9_, \.]+)')
    x_values = []
    y_values = []
    assignment = []
    actual_cluster = -1

    for line in csv_file:
        cluster_match = cluster_re.match(line)
        object_match = object_re.match(line)

        if object_match:
            object_values = object_match.group(2).split(',')
            dimension_number = 1

            for value in object_values:
                if dimension_number in dimensions_taken_into_account:
                    if len(x_values) == len(y_values):
                        x_values.append(float(value))
                    else:
                        y_values.append(float(value))
                dimension_number += 1

            assignment.append(actual_cluster)
        elif cluster_match:
            actual_cluster = int(re.search('\d+', cluster_match.group(0)).group(0))

    pyplot.subplot(211)
    pyplot.scatter(x_values, y_values, c=assignment)
    pyplot.subplot(212)
    pyplot.hist(assignment, facecolor='green', alpha=0.75)
    pyplot.show()

