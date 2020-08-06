#!/bin/bash

set -e

if [ x${7} = x ]; then
  echo "Usage: ${0} version yrst mnst dyst yred mned dyed"
  exit
fi

ver=${1}
yrst=${2}
mnst=${3}
dyst=${4}
yred=${5}
mned=${6}
dyed=${7}

rm -f namelist.create_rain

sed -e s%@yrst@%${yrst}% \
    -e s%@mnst@%${mnst}% \
    -e s%@dyst@%${dyst}% \
    -e s%@yred@%${yred}% \
    -e s%@mned@%${mned}% \
    -e s%@dyed@%${dyed}% \
    namelist.create_rain_${ver}_update_template > namelist.create_rain

./precip_minus_snow_interannual > create_rain-${ver}_update.log

rm -f namelist.create_rain
