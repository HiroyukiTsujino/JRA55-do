#! /bin/sh
#
set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} dataset_name (org c0 c1 c2 ...)"
  exit
fi

dsname=${1}

#export OMP_NUM_THREADS=2

ln -sf NAMELIST.MXE.COBESST.ocean NAMELIST.MXE.COBESST

ln -sf namelist.diagsurf.${dsname} namelist.diagsurf

./diag_surf_interannual_on_sst
