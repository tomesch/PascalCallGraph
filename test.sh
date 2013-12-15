#!/bin/sh
rm ./tests/results/*
for file in `ls ./tests/`
do
    ./parser ./tests/$file > ./tests/results/$file.dot
    dot -Tpng ./tests/results/$file.dot -o ./tests/results/$file.png
done