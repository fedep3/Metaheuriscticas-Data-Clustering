#!/bin/bash

echo "IMG"

for (( i=0; i<30; i++ ))
do
    ./parser TGA final_img.sql ../Tests/SalidasFinales/imgs/genetico${i}.result
done

for (( i=0; i<30; i++ ))
do
    ./parser TPSO final_img.sql ../Tests/SalidasFinales/imgs/pso${i}.result
done

for (( i=0; i<30; i++ ))
do
    ./parser TWPSO final_img.sql ../Tests/SalidasFinales/imgs/wpso${i}.result
done

for (( i=0; i<30; i++ ))
do
    ./parser TDE final_img.sql ../Tests/SalidasFinales/imgs/de${i}.result
done

for (( i=0; i<30; i++ ))
do
    ./parser TSDE final_img.sql ../Tests/SalidasFinales/imgs/sde${i}.result
done

for (( i=0; i<30; i++ ))
do
    ./parser TBee final_img.sql ../Tests/SalidasFinales/imgs/abeja${i}.result
done

for (( i=0; i<30; i++ ))
do
    ./parser TAnt final_img.sql ../Tests/SalidasFinales/imgs/hormiga${i}.result
done

#CSV

echo "CSV"

for (( i=0; i<30; i++ ))
do
    ./parser TGA final_csv.sql ../Tests/SalidasFinales/csvs/genetico${i}.result
done

for (( i=0; i<30; i++ ))
do
    ./parser TPSO final_csv.sql ../Tests/SalidasFinales/csvs/pso${i}.result
done

for (( i=0; i<30; i++ ))
do
    ./parser TWPSO final_csv.sql ../Tests/SalidasFinales/csvs/wpso${i}.result
done

for (( i=0; i<30; i++ ))
do
    ./parser TDE final_csv.sql ../Tests/SalidasFinales/csvs/de${i}.result
done

for (( i=0; i<30; i++ ))
do
    ./parser TSDE final_csv.sql ../Tests/SalidasFinales/csvs/sde${i}.result
done

for (( i=0; i<30; i++ ))
do
    ./parser TBee final_csv.sql ../Tests/SalidasFinales/csvs/abeja${i}.result
done

for (( i=0; i<30; i++ ))
do
    ./parser TAnt final_csv.sql ../Tests/SalidasFinales/csvs/hormiga${i}.result
done
