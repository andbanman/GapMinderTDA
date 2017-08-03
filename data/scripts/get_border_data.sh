#!/bin/bash
# Generates a listing of border countries 
# Note that not all country codes return a valid listing

countries="raw/country_codes.csv"
output="borders.csv"
rm $output

country_codes=$(cat $countries | egrep -v "s/,$/" | cut -f2 -d,)
done_codes=""

for country in $country_codes; do
	result=""
	neighbors=""
	tmp="$country.data"

    # Skip already processed codes
    if echo $done_codes | grep -qs $country; then continue; fi

	# pull data from geonames database
	curl "http://api.geonames.org/neighboursJSON?country=$country&username=abanman" > $tmp 2>/dev/null

	# split the output into lines
	sed -i 's/adminCode/  /' $tmp 2>/dev/null
    for word in $(cat $tmp); do
        echo $word >> $tmp.2
    done

	# pull out neighbor codes
    neighbors=$(grep "countryCode" $tmp.2 > $tmp.3)
	neighbors="$(sed -e "s/.*\"countryCode\":\"\(..\)\".*/\1/" $tmp.3)"

    for neighbor in $neighbors; do
        result="$neighbor $result"
    done
	echo "found $country neighbors: $result"
	echo "$country, $result" >> $output

    done_codes+=" $country"
	rm $tmp*
done
