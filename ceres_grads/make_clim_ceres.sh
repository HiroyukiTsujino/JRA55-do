#!/bin/bash

set -e

if [ x${1} = x ]; then
  echo "Usage ${0} period (2001_2012, 2000_2015)"
  exit
fi

period=${1}

#period=2001_2009
#period=2000_2015
#period=2001_2012

#for item in dswrf
for item in dswrf_seaice dlwrf
do
  ln -sf namelist.monthly_clim_ceres_${item}_${period} namelist.monthly_clim
  ./mkmonclim
done

rm -f namelist.monthly_clim
