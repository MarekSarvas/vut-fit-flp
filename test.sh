#!/bin/bash
# Project: FLP project 1 - BKG-2-CNF test script
# Author: Marek Sarvas
# Login: xsarva00
# Date: 2021/2022

if [ $# -ne 1 ]; then
    echo "Invalid number of arguments, select option to test, e.g. ${$0} [i|1|2|e]"
fi

option=$1
proj=flp21-fun
TEST_PATH=./test

prefix=""
if [ $option = "i" ]; then
    prefix="-"
fi
# compile project
#make
correct=0
all=0
if [ $option != "e" ];then
    echo "Testing ${prefix}${option} option."
    tests=$(ls test | grep -E '[0-9]+.in')
    for test_in in $tests; do
        ((all=all+1))
        test_out=$(echo "${test_in/.in/_${option}.out}" )

        # run programme
        ./$proj ${prefix}${option} ${TEST_PATH}/${test_in} > ${TEST_PATH}/tmp
        # check nonterminals
        diff <(head -n 1 ${TEST_PATH}/tmp | sort) <(head -n 1 ${TEST_PATH}/${test_out} | sort) > /dev/null
        if [ $(echo $?) -ne 0 ]; then
            echo "Wrong nonterminals"
            continue
        fi
        # check terminals
        diff <(head -n 2 ${TEST_PATH}/tmp | tail -n +2 | sort) <(head -n 2 ${TEST_PATH}/${test_out} | tail -n +2 | sort) > /dev/null
        if [ $(echo $?) -ne 0 ]; then
            echo "Wrong terminals"
            continue
        fi
        #check starting symbol
        diff <(head -n 3 ${TEST_PATH}/tmp | tail -n +3 | sort) <(head -n 3 ${TEST_PATH}/${test_out} | tail -n +3 | sort) > /dev/null
        if [ $(echo $?) -ne 0 ]; then
            echo "Incorrect starting symbol"
            continue
        fi
        # check rules
        diff <(tail -n +4 ${TEST_PATH}/tmp | sort) <(tail -n +4 ${TEST_PATH}/${test_out} | sort)
        if [ $(echo $?) -ne 0 ]; then
            echo "Incorrect rules"
            continue
        fi
        ((correct=correct+1))
    done
    echo "Correct ${correct}/${all}"
fi

if [ $option == "e" ];then
    echo "Testing for wrong inputs"
    tests=$(ls test | grep err.in)
    for test_in in $tests; do
        ((all=all+1))
        test_out=$(echo "${test_in/.in/_${option}.out}" )
        # run programme
        ./$proj -i ${TEST_PATH}/${test_in} > ${TEST_PATH}/tmp > /dev/null 2>&1
        # if caught error add as correct test
        if [ $(echo $?) -ne 0 ]; then
            ((correct=correct+1))
        fi
    done
    echo "Correct ${correct}/${all}"
fi
