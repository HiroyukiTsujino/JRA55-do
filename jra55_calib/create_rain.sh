#!/bin/bash -f

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} version"
  exit
fi

ver=${1}

rm -f namelist.create_rain

ln -sf namelist.create_rain_${ver} namelist.create_rain
./precip_minus_snow_interannual > create_rain-${ver}.log

rm -f namelist.create_rain
