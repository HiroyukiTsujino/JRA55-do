#! /bin/sh
#
set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} dataset_name (org c0 c1 c2 ...)"
  exit
fi

dsname=${1}

ln -sf namelist.diagflux.${dsname} namelist.diagflux

export OMP_NUM_THREADS=4

./diag_bulk_interannual_on_sst
