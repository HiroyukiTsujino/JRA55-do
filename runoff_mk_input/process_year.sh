#!/bin/bash

set -e

if [ x${1} == x ]; then
   echo "Usage: ${0} year dayend"
   exit
fi

year=${1}
dyed=${2}

./CONV_GRIB2bin1dy_mri.sh  ${year} ${year} 1 ${dyed}

./CONV_T319toRIV1dy_tfact_mri.sh  ${year} ${year} 1 ${dyed}
