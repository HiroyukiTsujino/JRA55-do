#!/bin/sh

set -e

if [ x${2} == x ]; then
   echo "Usage: ${0} year number_of_days"
   exit
fi

year=${1}
dayed=${2}

for item in slprs ice brtmp tmp10m sph10m u10m v10m dswrf dlwrf prcp snow
do
  ./make_TL319_latestyear_v1_5.sh ${item} ${year} ${year} ${dayed}
done
