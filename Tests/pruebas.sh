#!/bin/bash

lc[1]=5
lc[2]=7
lc[3]=9
lc[4]=11

pc[1]=4
pc[2]=6
pc[3]=7
pc[4]=11

cac[1]=4
cac[2]=5
cac[3]=11
cac[4]=7

rm .log 2> /dev/null

for (( i=1; i<5; i++ ))
do
    for (( j=1; j<31; j++ ))
    do
        echo "GA LENNA CLUSTERS ${lc[${i}]} CORRIDA ${j}"
        ./runTests GAL${lc[${i}]} TestsPublicacion/galenna${lc[${i}]}.test
        ../Parser/parser TGA ../Parser/GAL${lc[${i}]}.sql GAL${lc[${i}]}.result
        rm .log
    done
done

for (( i=1; i<5; i++ ))
do
    for (( j=1; j<31; j++ ))
    do
        echo "GA PEPPERS CLUSTERS ${pc[${i}]} CORRIDA ${j}"
        ./runTests GAP${pc[${i}]} TestsPublicacion/gapepper${pc[${i}]}.test
        ../Parser/parser TGA ../Parser/GAP${pc[${i}]}.sql GAP${pc[${i}]}.result
        rm .log
    done
done

for (( i=1; i<5; i++ ))
do
    for (( j=1; j<31; j++ ))
    do
        echo "GA CAMERAMAN CLUSTERS ${cac[${i}]} CORRIDA ${j}"
        ./runTests GAC${cac[${i}]} TestsPublicacion/gacameraman${cac[${i}]}.test
        ../Parser/parser TGA ../Parser/GAC${cac[${i}]}.sql GAC${cac[${i}]}.result
        rm .log
    done
done

for (( j=1; j<31; j++ ))
do
    echo "BEE LENNA CLUSTERS ${lc[1]} CORRIDA ${j}"
    ./runTests BEEL${lc[1]} TestsPublicacion/beelenna${lc[1]}.test
    ../Parser/parser TBee ../Parser/BEEL${lc[1]}.sql BEEL${lc[1]}.result
    rm .log
done

for (( j=1; j<31; j++ ))
do
    echo "BEE PEPPERS CLUSTERS ${lc[1]} CORRIDA ${j}"
    ./runTests BEEP${lc[1]} TestsPublicacion/beepepper${lc[1]}.test
    ../Parser/parser TBee ../Parser/BEEP${lc[1]}.sql BEEP${lc[1]}.result
    rm .log
done

for (( i=1; i<4; i++ ))
do
    for (( j=1; j<31; j++ ))
    do
        echo "BEE CAMERAMAN CLUSTERS ${cac[${i}]} CORRIDA ${j}"
        ./runTests BEEC${cac[${i}]} TestsPublicacion/beecameraman${cac[${i}]}.test
        ../Parser/parser TBee ../Parser/BEEC${cac[${i}]}.sql BEEC${cac[${i}]}.result
        rm .log
    done
done


cd ..
cd Parser
./genPubTables.py
pdflatex tablas.tex
mv table.*.tex tables
cd ..
cd Tests
