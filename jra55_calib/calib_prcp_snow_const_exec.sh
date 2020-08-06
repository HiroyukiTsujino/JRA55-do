#!/bin/bash

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} version"
  exit
fi

ver=${1}

rm -f namelist.calibpcp

ln -sf namelist.calibpcp.const_${ver} namelist.calibpcp
./calib_precip_interannual > calib_prcp_const_${ver}.log

ln -sf namelist.calibpcp.snow_const_${ver} namelist.calibpcp
./calib_precip_interannual > calib_snow_const_${ver}.log

rm -f namelist.calibpcp
