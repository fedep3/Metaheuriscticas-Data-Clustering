#!/bin/bash

cac[1]=4
cac[2]=5
cac[3]=7
cac[4]=11

for (( i=1; i<5; i++ ))
do
    for (( j=1; j<6; j++ ))
    do
        echo "GA CAMERAMAN CLUSTERS ${cac[${i}]} CORRIDA ${j}"
        ./runTests GAC${cac[${i}]} OptPublicacion/gacameraman${i}.test
        ../Parser/parser TGA ../Parser/GAC${cac[${i}]}.sql GAC${cac[${i}]}.result
        rm .log
    done
done

#echo "BEE CAMERAMAN CLUSTERS 4 CORRIDA 1"
#./runTests BEEC4 OptPublicacion/beecameraman4.test
#../Parser/parser TBee ../Parser/BEEC4.sql BEEC4.result
#rm .log

#echo "BEE CAMERAMAN CLUSTERS 5 CORRIDA 1"
#./runTests BEEC5 OptPublicacion/beecameraman5.test
#../Parser/parser TBee ../Parser/BEEC5.sql BEEC5.result
#rm .log
