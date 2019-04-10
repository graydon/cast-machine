#! /bin/sh

files=$(ls ./tests)

for f in ${files}; do
    echo "testing $f"
    ./cast.exe --load "./tests/$f"
    echo ""
done
