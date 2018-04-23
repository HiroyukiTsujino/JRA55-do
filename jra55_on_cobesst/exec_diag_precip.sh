#! /bin/sh

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} dataset_name (v1_1 v0_8 ...)"
  exit
fi

dsname=${1}

ln -sf NAMELIST.MXE.COBESST.ocean NAMELIST.MXE.COBESST

ln -sf namelist.diagpcp.${dsname} namelist.diagpcp

./diag_precip_interannual_on_sst
