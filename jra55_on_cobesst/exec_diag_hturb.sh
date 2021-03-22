#! /bin/sh

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} dataset_name (org v0_1 ...)"
  exit
fi

dsname=${1}

#export OMP_NUM_THREADS=4

ln -sf NAMELIST.MXE.COBESST.ocean NAMELIST.MXE.COBESST

ln -sf namelist.diaghturb.${dsname} namelist.diaghturb

./diag_hturb_interannual_on_sst
