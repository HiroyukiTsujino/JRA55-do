#!/bin/bash -f

set -e

if [ x${6} == x ]; then
   echo "Usage: ${0} yrst mnst dyst yred mned dyed"
   exit
fi

yrst=${1}
mnst=${2}
dyst=${3}
yred=${4}
mned=${5}
dyed=${6}

sed -e s%@yrst@%${yrst}% \
    -e s%@mnst@%${mnst}% \
    -e s%@dyst@%${dyst}% \
    -e s%@yred@%${yred}% \
    -e s%@mned@%${mned}% \
    -e s%@dyed@%${dyed}% \
    namelist.filterwind.jra55.latest_template > namelist.filterwind.jra55.latest

ln -sf namelist.filterwind.jra55.latest namelist.filterwind

./filter_wind_interannual

rm -f namelist.filterwind
