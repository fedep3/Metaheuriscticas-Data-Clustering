#!/bin/bash

rm .log 2> /dev/null

date
for (( i=1; i<31; i++ ))
do
    echo "GA LENNA DB ${i}"
    ./runTests GALDB second_tests_db.test
    rm .log
done

date
for (( i=1; i<31; i++ ))
do
    echo "GA LENNA TURI ${i}"
    ./runTests GALTURI second_tests_turi.test
    rm .log
done
