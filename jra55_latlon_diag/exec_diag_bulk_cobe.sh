#! /bin/sh
#
set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} bulk_formula dataset_version"
  exit
fi

bulk=${1}
ver=${2}

ln -sf namelist.diagbulkflx_cobe_${bulk}_${ver} namelist.diagbulkflx_cobe

export OMP_NUM_THREADS=4

./diag_bulk_interannual_on_cobesst
