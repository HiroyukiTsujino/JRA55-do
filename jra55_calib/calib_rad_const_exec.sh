#!/bin/bash -f

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} version"
  exit
fi

ver=${1}

ln -sf namelist.calibrad.const_${ver} namelist.calibrad

./calib_rad_interannual > calib_rad_const_${ver}.log

\rm namelist.calibrad
