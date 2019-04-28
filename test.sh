#! /bin/sh

files=$(ls ./tests)

for f in ${files}; do
    echo "testing $f"

    out="- :$(cat "tests/$f" | grep "(\*\*" | awk '{$1="";$NF=""; print $0}')"
    echo "expected output:"
    echo "{${out}}"

    rout="$(./cast.exe --load "./tests/$f") "
    echo "computed output:"
    echo "{${rout}}"

    if [ "${out}" = "${rout}" ]; then
	echo "success"
    else
	echo "wrong output"
	echo "terminating test session"
	exit
    fi
done
