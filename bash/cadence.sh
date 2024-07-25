#!/bin/bash

(   
    echo "Ziel,cadence"
    grep 'cadence' "$1" \
    | sed 's#</.*##; s#.*>#'$2',#' 
) > "../data/$2.csv"
