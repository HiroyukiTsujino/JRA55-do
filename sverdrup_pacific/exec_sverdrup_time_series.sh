#! /bin/sh

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} dataset_name (v1_2 v1_4pre1 ...)"
  exit
fi

dsname=${1}

ln -sf NAMELIST.MXE.COBESST.PACSV NAMELIST.MXE

ln -sf namelist.sverdrup.${dsname} namelist.sverdrup

./sverdrup_time_series
