#!/bin/bash

set -e

if [ x${1} = x ]; then
  echo "Usage ${0} period (mar2000_feb2015 or jan2001_dec2012)"
  exit
fi

period=${1}

for item in dswrf dlwrf
do
  for mon in `seq -w 1 12`
  do
    sed -e s%@month@%${mon}% \
        -e s%@period@%${period}% \
      namelist.filter_clim_${item}_template > namelist.filter_clim
    ./filter_rad_clim
  done
done

rm -f namelist.filter_clim
