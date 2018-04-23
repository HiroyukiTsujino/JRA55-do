#! /bin/sh

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} bulk_formula"
  exit
fi

bulk=${1}

ln -sf namelist.diagbulkflx_cobe_${bulk} namelist.diagbulkflx_cobe

#export OMP_NUM_THREADS=4

./diag_bulk_interannual_on_cobesst
