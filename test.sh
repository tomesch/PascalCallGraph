#!/bin/sh
rm ./tests/results/*.dot
rm ./tests/results/*.png
for file in `find ./tests/*.p -type f -maxdepth 1 \( ! -iname ".*" \) | sed 's#.*/##'`
do
    ./parser ./tests/$file > ./tests/results/$file.dot
    dot -Tpng ./tests/results/$file.dot -o ./tests/results/$file.png
done
