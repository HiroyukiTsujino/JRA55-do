#! /bin/sh

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} dataset_name (org v0_1 ...)"
  exit
fi

dsname=${1}

ln -sf NAMELIST.MXE.COBESST.ocean NAMELIST.MXE.COBESST

ln -sf namelist.diagnetrad.${dsname} namelist.diagnetrad

./diag_netrad_interannual_on_sst
