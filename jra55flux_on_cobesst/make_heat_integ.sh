#!/bin/bash

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} dataset_name"
  exit
fi

ds_name=${1}

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE
ln -sf namelist.heatall.${ds_name} namelist.heatall

./surf_heat_all_ann

exit 0
