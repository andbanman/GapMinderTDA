#!/bin/bash

borders_f=$1
codes_f=raw/country_codes.csv

if [[ ! -f $borders_f ]]; then
    echo "$borders_f: file does not exist"
    exit 1
fi



while read line; do
	country=${line%,*}
	neighbors=${line#*,}

    if [[ $line = '#'* ]]; then
        continue;
    fi

	for neighbor in $neighbors; do
        # check that the country in question is a neighbor of
        # each of its neighbors
        if ! grep -qs "^$neighbor,.*$country" $borders_f; then
            echo "ERROR: $country is not a neighbor of $neighbor"
            if ! grep -qs "$neighbor" $codes_f; then
                echo "ERROR: $neighbor not listed in $codes_f"
            fi
        fi
    done
done < $borders_f
