#!/bin/bash

set -e

if [ x${1} == x ]; then
   echo "Usage: ${0} year"
   exit
fi

year=${1}

################

for item in slprs u10m v10m dswrf prcp
do
  sed -e s%@year@%${year}% ctl_v1_3/${item}.ctl.template > ctl_v1_3/${item}.${year}.ctl
  sed -e s%@year@%${year}% ctl_v1_5/${item}.ctl.template > ctl_v1_5/${item}.${year}.ctl
done

for item in prmsl uwnd.10m vwnd.10m dswrf.sfc prate
do
  sed -e s%@year@%${year}% ctl_20CRv3/${item}.ctl.template > ctl_20CRv3/${item}.${year}.ctl
done
