#! /bin/sh
#
set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} dsversion"
fi

ver=${1}

ln -sf namelist.diagbulkflx_${ver} namelist.diagbulkflx

export OMP_NUM_THREADS=4

./diag_bulk_interannual_on_brtmp
