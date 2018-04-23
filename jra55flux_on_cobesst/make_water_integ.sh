#!/bin/bash -f

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} dataset_version"
  exit
fi

ver=${1}

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

rm -f namelist.waterall

ln -sf namelist.waterall.${ver} namelist.waterall

./surf_water_all_ann
