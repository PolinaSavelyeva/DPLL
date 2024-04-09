#!/bin/bash

mysat_ans=$(mktemp)
unsat_msg="s UNSATISFIABLE"

dotnet run "$1" > "$mysat_ans"
echo mysat answer:; cat "$mysat_ans"; echo 

if [ "$(cat "$mysat_ans")" = "$unsat_msg" ]; then
    rm "$mysat_ans"
    picosat_ans=$(mktemp)
    picosat "$1" > "$picosat_ans"
    
    echo picosat answer:; cat "$picosat_ans"; echo

    if [ "$(cat "$picosat_ans")" = "$unsat_msg" ]; then
        echo "PASSED"
        rm "$picosat_ans"
        exit 0
    else
        echo "FAILED"
        rm "$picosat_ans"
        exit 1
    fi
else
    new_cnf=$(mktemp)
    cp "$1" "$new_cnf"
    num=0
    
    for i in $(sed 1d "$mysat_ans"); do
        if [ "$i" != "v" ] && [ "$i" != "0" ]; then
            echo "$i" 0 >> "$new_cnf"
            num=$(expr "$num" + 1)
        fi
    done

    awk -v num="$num" '/^p cnf/ { 
        $4 += num; 
        print $0; 
        next 
    } 
    { print }' "$new_cnf" > "$new_cnf".tmp
    
    mv "$new_cnf".tmp "$new_cnf"
    
    picosat_ans=$(mktemp)
    picosat "$new_cnf" > "$picosat_ans"
    rm "$new_cnf"
    
    echo "picosat answer:"; cat "$picosat_ans"; echo

    if [ "$(cat "$picosat_ans" | tr -d '\n' | tr -d 'v')" = "$(cat "$mysat_ans" | tr -d '\n' | tr -d 'v')" ]; then
        echo "PASSED"      
        rm "$picosat_ans"
        exit 0
    else
        echo "FAILED"
        rm "$picosat_ans"
        exit 1      
    fi
fi
