#!/bin/bash

if [[ "$#" -ne 1 ]]; then
    echo "Usage: $0 [filename]"
    exit 1
fi

if [[ ! -f $1 ]]; then
    echo "File does not exist: $1"
    exit 1
fi

sum=$(cat $1 | xargs -I % sh -c 'echo $(( %/3 - 2 ))' | awk 'BEGIN { sum=0 } { sum+=$1 } END {print sum}')

echo "Fuel needed: $sum"
