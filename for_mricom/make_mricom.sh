#! /bin/sh

set -e

if [ x${2} == x ]; then
   echo "Usage: ${0} start_year end_year"
   exit
fi

yearst=${1}
yeared=${2}

#for item in dswrf dlwrf tmp2m sph2m u10m v10m prcp # v01 (raw)
#
#for item in slprs
#for item in ice brtmp
#for item in tmp10m sph10m
#for item in u10m v10m
#for item in dswrf dlwrf
#for item in prcp snow
do
  ./make_TL319.sh ${item} ${yearst} ${yeared}
done
