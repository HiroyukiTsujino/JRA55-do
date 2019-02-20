#!/bin/bash -f

set -e

if [ x${1} == x ]; then
   echo "Usage: ${0} year"
   exit
fi

year=${1}

./CONV_GRIB2bin1dy_mri.sh  ${year} ${year}

./CONV_T319toRIV1dy_tfact_mri.sh  ${year} ${year}
