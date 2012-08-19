#!/bin/bash

rm .log 2> /dev/null

for (( i=1; i<2; i++ ))
do
    echo "GA JET RUN ${i}"
    date
    ./runTests GAJ first_tests_jet.test
    rm .log
    echo "GA MANDRIL RUN ${i}"
    date
    ./runTests GAM first_tests_mandrill.test
    rm .log
done

shutdown -h now
