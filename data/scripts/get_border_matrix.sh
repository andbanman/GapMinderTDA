#!/bin/bash
#
# Convert country borders list to a NxN matrix, with countries indexed
# according to the following array.
#
# get_border_matrix.sh <borders.csv>
#
# Andrew Banman <banma001@umn.edu>

echo "Generating border_matrix.csv ..."

rm -f border_matrix.csv
rm -f border_matrix.csv.test

borders_f=$1

#declare codes
#while read line; do
#	index=$(echo $line | cut -f1 -d, | sed 's/"//g')
#    ((index = index - 1))
#	code=$(echo $line | cut -f2 -d, | sed 's/"//g')
#    codes[$index]=$code
#done < data_processed.csv
codes=$(cat data_processed.csv | cut -f2 -d,)

i=0
while read line; do
    if [[ $line = '#'* ]]; then
        continue;
    fi

	country=$(echo $line | cut -f2 -d,)      #get next country from data set
	neighbors=$(grep "^$country," $borders_f) # find its neighbors
	neighbors=${neighbors#*,}                     # strip the country
	line=""

	for code in $codes; do
		hit=false
		for neighbor in $neighbors; do
			if [ "$code" == $neighbor ]; then
				hit=true
                break
			fi
		done

		if [ $hit == true ]; then
            borders[$i]+=" 1"
			if [ "x$line" == "x" ]; then
				line="1"
			else
				line="$line,1"
			fi
		else
            borders[$i]=" 0"
			if [ "x$line" == "x" ]; then
				line="0"
			else
				line="$line,0"
			fi
		fi
	done

	echo $line >> border_matrix.csv
    ((i = i + 1))
done < data_processed.csv

exit 1
### test the symmetry of the border matrix
num=$(wc -l data_processed.csv | awk '{print $1}')
((num = num - 1))
for i in $(seq 0 $num); do
    line=""
    for j in $(seq 0 $num); do
        row_i=(${borders[$i]})
        row_j=(${borders[$j]})
        if [[ ${row_i[$j]} -ne ${row_j[$i]} ]]; then
            echo "ERROR border mismatch $i $j"
        fi
    done
    echo $line >> border_matrix.csv.test
done
