#!/bin/bash

if [[ "$#" -ne 1 ]]; then
    echo "Usage: $0 [filename]"
    exit 1
fi

if [[ ! -f $1 ]]; then
    echo "File does not exist: $1"
    exit 1
fi

print_fuel () {
    weight=$1;
    while [[ $weight > 0 ]]; do
        weight=$(( $weight / 3 - 2 ));
        if [[ $weight > 0 ]]; then
            echo $weight;
        fi;
    done
}

print_all_fuel () {
    while IFS= read -r next_module
    do
        print_fuel $next_module
    done < "$1"
}

sum=$(print_all_fuel $1 | awk 'BEGIN { sum=0 } { sum+=$1 } END {print sum}')

echo "Fuel needed: $sum"
