#!/bin/bash -f

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} version"
  exit
fi

ver=${1}

rm -f namelist.calibpcp

ln -sf namelist.calibpcp.med_${ver} namelist.calibpcp.med
./calib_precip_mediterranean > calib_prcp_med_${ver}.log

ln -sf namelist.calibpcp.snow.med_${ver} namelist.calibpcp.med
./calib_precip_mediterranean > calib_snow_med_${ver}.log

rm -f namelist.calibpcp.med
