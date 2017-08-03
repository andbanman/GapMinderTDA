#!/bin/bash
#creates a subset of the border data matching the countries found in the data set

borders_f=raw/borders.mod.csv
subset_f=borders.mod.subset.csv
codes_f=raw/country_codes.csv
data_f=data_processed.csv

cp $borders_f $subset_f
sed -i.bak "/^#/ d" $subset_f #remove comments
#rm -f $subset_f

echo "Generating border subset ..."

present_codes=$(cat $data_f | cut -f2 -d,)

# if country not in codes from the border matrix
while read line; do
    code=${line%,*}
    found=0
    # find if border line is relevant
    for present_code in $present_codes; do
        if [[ $code == $present_code ]]; then
            found=1
            break;
        fi
    done

    if [[ $found -ne 1 ]]; then
        sed -i.bak "/^$code,/ d" $subset_f
        sed -i.bak "s/$code//g" $subset_f
    fi
done < $borders_f

rm $subset_f.bak
