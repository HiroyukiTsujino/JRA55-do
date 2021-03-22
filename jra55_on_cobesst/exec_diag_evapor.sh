#! /bin/sh

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} dataset_name (v0_1 v0_8 ...)"
  exit
fi

dsname=${1}

ln -sf NAMELIST.MXE.COBESST.ocean NAMELIST.MXE.COBESST

ln -sf namelist.diagevp.${dsname} namelist.diagevp

./diag_evap_interannual_on_sst
