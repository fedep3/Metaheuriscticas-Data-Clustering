#!/bin/bash

rm .log 2> /dev/null

for (( i=1; i<2; i++ ))
do
    echo "GA LENNA RUN ${i}"
    date
    ./runTests GALCOLOR first_color_tests.test
    rm .log
done
