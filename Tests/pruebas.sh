#!/bin/bash

lc[1]=5
lc[2]=7
lc[3]=9
lc[4]=11

pc[1]=4
pc[2]=6
pc[3]=7
pc[4]=11

rm .log 2> /dev/null

for (( i=1; i<5; i++ ))
do
    mkdir GAL${lc[${i}]}
    for (( j=1; j<31; j++ ))
    do
        echo "GA LENNA CLUSTERS ${lc[${i}]} CORRIDA ${j}"
        ./runTests GAL${lc[${i}]} TestsPublicacion/galenna${lc[${i}]}.test
        rename "s/GA0-imglennamediumtiff/GAL${lc[${i}]}${j}/" *.tiff
        mv *.tiff GAL${lc[${i}]}
        ../Parser/parser TGA ../Parser/GAL${lc[${i}]}.sql GAL${lc[${i}]}.result
        rm .log
    done
done

for (( i=1; i<5; i++ ))
do
    mkdir GAP${lc[${i}]}
    for (( j=1; j<31; j++ ))
    do
        echo "GA PEPPERS CLUSTERS ${pc[${i}]} CORRIDA ${j}"
        ./runTests GAP${pc[${i}]} TestsPublicacion/gapepper${pc[${i}]}.test
        rename "s/GA0-imgpeppersmediumtiff/GAP${pc[${i}]}${j}/" *.tiff
        mv *.tiff GAP${pc[${i}]}
        ../Parser/parser TGA ../Parser/GAP${pc[${i}]}.sql GAP${pc[${i}]}.result
        rm .log
    done
done

mkdir BEEL${lc[1]}
for (( j=1; j<31; j++ ))
do
    echo "BEE LENNA CLUSTERS ${lc[1]} CORRIDA ${j}"
    ./runTests BEEL${lc[1]} TestsPublicacion/beelenna${lc[1]}.test
    rename "s/Bee0-imglennamediumtiff/BEEL${lc[1]}${j}/" *.tiff
    mv *.tiff BEEL${lc[1]}
    ../Parser/parser TBee ../Parser/BEEL${lc[1]}.sql BEEL${lc[1]}.result
    rm .log
done

mkdir BEEP${lc[1]}
for (( j=1; j<31; j++ ))
do
    echo "BEE PEPPERS CLUSTERS ${lc[1]} CORRIDA ${j}"
    ./runTests BEEP${lc[1]} TestsPublicacion/beepepper${lc[1]}.test
    rename "s/Bee0-imgpeppersmediumtiff/BEEP${lc[1]}${j}/" *.tiff
    mv *.tiff BEEP${lc[1]}
    ../Parser/parser TBee ../Parser/BEEP${lc[1]}.sql BEEP${lc[1]}.result
    rm .log
done

cd ..
cd Parser
./genPubTables.py
pdflatex tablas.tex
mv table* tables
cd ..
cd Tests
