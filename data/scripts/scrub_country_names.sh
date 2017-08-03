#!/bin/bash
# replace country names with ISO codes

countries=raw/country_codes.csv
find raw -name "Data-Tabla*.csv" > files.tmp

echo "Copying raw data files ..."

# copy over raw files and do some pre-processing
echo "Replacing country names with ISO codes ..."

while read file; do
    newfile=${file#*/} #strip all leading to first slash
    newfile=${newfile%/Data*}.csv

    echo
    echo "$file >> $newfile"

    # Remove commas and quotes
    cp "$file" "$file.tmp"
    sed -i.bak 's/"\(.*\),\(.*\)"/\1\2/' "$file.tmp"
    sed -i.bak 's/"\(.*\),\(.*\)"/\1\2/' "$file.tmp"
    sed -i.bak 's/"\(.*\),\(.*\)"/\1\2/' "$file.tmp"
    sed -i.bak 's/"//' "$file.tmp"
    rm "$file.tmp.bak"

    i=0
    while read line; do
        (( i = i + 1 ))
        if [[ i -eq 1 ]]; then
            echo $line > $newfile
            continue
        fi

        name=${line%%,*}
        if [[ -z $name ]]; then
            continue;
        fi
        res=$(grep -i "^$name," $countries)
        if [[ -z $res ]]; then
            echo "ERROR: no country $name in $countries"
            continue
        else
            code=${res##*,}
            if [[ -z $code ]]; then
                echo "WARN: no ISO code for $name in $countries"
                continue
            else
                echo $line | sed "s/$name/$code/" >> $newfile
            fi
        fi
    done < "$file.tmp"
    rm "$file.tmp"
done < files.tmp
rm files.tmp

exit 0
