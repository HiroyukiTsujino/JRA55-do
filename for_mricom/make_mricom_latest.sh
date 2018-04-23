#! /bin/sh

set -e

if [ x${2} == x ]; then
   echo "Usage: ${0} year number_of_days"
   exit
fi

year=${1}
dayed=${2}

#for item in dswrf dlwrf tmp2m sph2m u10m v10m prcp # v01
#
#for item in slprs
#for item in ice brtmp
#for item in tmp10m sph10m
#for item in u10m v10m
#for item in dswrf dlwrf
#for item in prcp snow
do
  ./make_TL319_latestyear.sh ${item} ${year} ${year} ${dayed}
done
