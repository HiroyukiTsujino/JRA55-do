#!/bin/bash -f

set -e

if [ x${1} = x ]; then
  echo "Usage ${0} version (v1,v2,v3)"
  exit
fi

ver=${1}

rm -f namelist.calibwindmag

ln -sf namelist.calibwindmag_${ver} namelist.calibwindmag

./calib_windmag_interannual
