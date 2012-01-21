for (( i=1; i<5; i++ ))
do
    for (( j=1; j<6; j++ ))
    do
        echo "GA LENNA TANDA ${i} CORRIDA ${j}"
        ./runTests GAL${i}${j} OptPublicacion/galenna${i}.test
        mkdir GAL${i}${j}
        cp *.tiff GAL${i}${j}
        rm .log
        ../Parser/parser TGA ../Parser/GAL${i}${j}.sql GAL${i}${j}.result
    done
done

for (( i=1; i<5; i++ ))
do
    for (( j=1; j<6; j++ ))
    do
        echo "GA PEPPERS TANDA ${i} CORRIDA ${j}"
        ./runTests GAP${i}${j} OptPublicacion/gapeppers${i}.test
        mkdir GAP${i}${j}
        cp *.tiff GAP${i}${j}
        rm .log
        ../Parser/parser TGA ../Parser/GAP${i}${j}.sql GAP${i}${j}.result
    done
done

for (( i=1; i<5; i++ ))
do
    for (( j=1; j<6; j++ ))
    do
        echo "BEE LENNA TANDA ${i} CORRIDA ${j}"
        ./runTests BEEL${i}${j} OptPublicacion/beelenna${i}.test
        mkdir BEEL${i}${j}
        cp *.tiff BEEL${i}${j}
        rm .log
        ../Parser/parser TBee ../Parser/BEEL${i}${j}.sql BEEL${i}${j}.result
    done
done

for (( i=1; i<5; i++ ))
do
    for (( j=1; j<6; j++ ))
    do
        echo "BEE PEPPERS TANDA ${i} CORRIDA ${j}"
        ./runTests BEEP${i}${j} OptPublicacion/beepeppers${i}.test
        mkdir BEEP${i}${j}
        cp *.tiff BEEP${i}${j}
        rm .log
        ../Parser/parser TBee ../Parser/BEEP${i}${j}.sql BEEP${i}${j}.result
    done
done

