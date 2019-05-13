#! /bin/sh

files=$(ls ./tests)
count=0
hangmode=0

for f in ${files}; do
    echo "testing $f"

    out="$(string="$(cat ./tests/$f | grep "(\*\*")" && echo "${string:4:-4}")"

    if [ "$out" = "Hangs" ]; then
        hangmode=1
    fi
    
    if [ "$out" ]; then
        out="- : $out"
    fi
    echo "expected output:"
    echo "{${out}}"
    
    if (( hangmode == 1 )); then
        echo "cannot test hanging programs yet"
        hangmode=0
        sleep 1
        continue
        # ./run.sh 5 ./cast.exe --load "./tests/$f" &
        # status=$?

        # if (( status == 124 )); then
        #     echo "hung program as expected"
        #     hangmode=0
        #     echo "success"
        #     count=$((count+1))
        #     continue

        # else
        #     echo "error: program should hang"
        #     echo "terminating test session"
        #     exit
        # fi
    fi

    # if the program should not hang
    rout="$(./cast.exe --load "./tests/$f" "$@")"

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
done

echo "all ${count} tests succeeded"