#!/bin/bash

rm .log 2> /dev/null

date
for (( i=1; i<31; i++ ))
do
    echo "GA MANDRILL DB ${i}"
    ./runTests GAMDB second_tests_mandrill_db.test
    rm .log
done

date
for (( i=1; i<31; i++ ))
do
    echo "GA MANDRILL TURI ${i}"
    ./runTests GAMTURI second_tests_mandrill_turi.test
    rm .log
done

date
for (( i=1; i<31; i++ ))
do
    echo "GA JET DB ${i}"
    ./runTests GAJDB second_tests_jet_db.test
    rm .log
done

date
for (( i=1; i<31; i++ ))
do
    echo "GA JET TURI ${i}"
    ./runTests GAJTURI second_tests_jet_turi.test
    rm .log
done

./runTests KMEANSM kmeans_mandrill.test
rm .log
./runTests KMEANSJ kmeans_jet.test
rm .log

cp KMEANSM.db KMEANSMDB.db
cp KMEANSJ.db KMEANSJDB.db

cp KMEANSM.db KMEANSMTURI.db
cp KMEANSJ.db KMEANSJTURI.db

rm KMEANSM.db KMEANSJ.db
