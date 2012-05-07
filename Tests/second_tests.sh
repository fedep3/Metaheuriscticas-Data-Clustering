#!/bin/bash

rm .log 2> /dev/null

echo "GA LENNA DB"
date
./runTests GALDB second_tests_db.test
rm .log

echo "GA LENNA TURI"
date
./runTests GALTURIsecond_tests_turi.test
rm .log
