#!/bin/bash

rm .log 2> /dev/null

for (( i=1; i<5; i++ ))
do
    echo "GA LENNA CORRIDA ${i}"
    date
    ./runTests GAL first_tests.test
    rm .log
done
