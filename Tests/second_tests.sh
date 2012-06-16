#!/bin/bash

rm .log 2> /dev/null

date
for (( i=1; i<31; i++ ))
do
    echo "GA LENNA DB ${i}"
    ./runTests GALDB second_tests_lenna_db.test
    rm .log
done

date
for (( i=1; i<31; i++ ))
do
    echo "GA LENNA TURI ${i}"
    ./runTests GALTURI second_tests_lenna_turi.test
    rm .log
done

date
for (( i=1; i<31; i++ ))
do
    echo "GA PEPPERS DB ${i}"
    ./runTests GAPDB second_tests_peppers_db.test
    rm .log
done

date
for (( i=1; i<31; i++ ))
do
    echo "GA PEPPERS TURI ${i}"
    ./runTests GAPTURI second_tests_peppers_turi.test
    rm .log
done

date
for (( i=1; i<31; i++ ))
do
    echo "GA CAMERAMAN DB ${i}"
    ./runTests GACDB second_tests_cameraman_db.test
    rm .log
done

date
for (( i=1; i<31; i++ ))
do
    echo "GA CAMERAMAN TURI ${i}"
    ./runTests GACTURI second_tests_cameraman_turi.test
    rm .log
done

./runTests KMEANSL kmeans_lenna.test
rm .log
./runTests KMEANSP kmeans_peppers.test
rm .log
./runTests KMEANSC kmeans_cameraman.test
rm .log

cp KMEANSL.db KMEANSLDB.db
cp KMEANSP.db KMEANSPDB.db
cp KMEANSC.db KMEANSCDB.db

cp KMEANSL.db KMEANSLTURI.db
cp KMEANSP.db KMEANSPTURI.db
cp KMEANSC.db KMEANSCTURI.db

rm KMEANSL.db KMEANSP.db KMEANSC.db
