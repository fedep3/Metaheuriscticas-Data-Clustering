#!/bin/bash

rm .log 2> /dev/null

for (( i=1; i<17; i++ ))
do
    echo "GA LENNA RUN ${i}"
    date
    ./runTests GAL first_tests.test
    rm .log
done