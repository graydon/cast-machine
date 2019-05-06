#! /bin/sh

files=$(ls ./tests)
count=0

for f in ${files}; do
    echo "testing $f"

    out="$(cat "tests/$f" | grep "(\*\*" | awk '{$1="";$NF=""; print $0}')"
    if [ "$out" ]; then
        out="- :$out"
    else 
        out=" "
    fi
    echo "expected output:"
    echo "{${out}}"

    rout="$(./cast.exe --load "./tests/$f") "
    echo "computed output:"
    echo "{${rout}}"

    if [ "${out}" = "${rout}" ]; then
	    echo "success"
        count=$((count+1))
    else
        echo "wrong output"
        echo "terminating test session"
        exit
    fi

    echo "all ${count} tests succeeded"
done
