#!/bin/bash

rm .log 2> /dev/null

for (( i=1; i<6; i++ ))
do
    echo "GA LENNA CORRIDA ${i}"
    ./runTests GAL first_tests.test
    rm .log
done
