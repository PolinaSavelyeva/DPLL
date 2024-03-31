#!/bin/bash

tmp=`mktemp`
dotnet run $1 > $tmp
ans_mysat=`head -1 $tmp`
unsat_msg="s UNSATISFIABLE"

echo mysat answer: 
cat $tmp; echo 

if [ "$ans_mysat" = "$unsat_msg" ]; then
    ans_picosat=`picosat $1`
    
    echo "picosat answer: $ans_picosat"

    if [ "$ans_picosat" = "$unsat_msg" ]; then
        echo "PASSED"
        rm "$tmp"
        exit 0
    else
        echo "FAILED"
        rm "$tmp"
        exit -1
    fi
else
    cnf_tmp=`mktemp`
    cp $1 "$cnf_tmp"
    model_mysat=`tail -1 $tmp`
    num=0
    
    for i in $model_mysat; do
        if [ $i != "v" ]; then
            if [ $i != "0" ]; then
                echo $i 0 >> $cnf_tmp
                num=`expr $num + 1`
            fi
        fi
    done

    awk -v num=$num '/^p cnf/ { 
        $4 += num; 
        print $0; 
        next 
    } 
    { print }' $cnf_tmp > $cnf_tmp.tmp
    mv $cnf_tmp.tmp $cnf_tmp
    
    tmp_picosat_ans=`mktemp`
    picosat $cnf_tmp > $tmp_picosat_ans
    model_picosat=`tail -1 $tmp_picosat_ans`
    
    echo "picosat answer:"
    cat $tmp_picosat_ans; echo
    
    if [ "$model_picosat" = "$model_mysat" ]; then
        echo "PASSED"
        rm $cnf_tmp
        rm $tmp_picosat_ans
        exit 0
    else
        echo "FAILED"
        rm $cnf_tmp
        rm $tmp_picosat_ans
        exit -1      
    fi
fi
