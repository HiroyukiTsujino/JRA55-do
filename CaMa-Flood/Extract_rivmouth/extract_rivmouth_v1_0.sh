#!/bin/bash

set -e

if [ x${2} == x ]; then
   echo "Usage: ${0} start_year end_year"
   exit
fi

yrst=${1}
yred=${2}

exe=mk_runoff_v1.0_mri

rm -f namelist.extract_rivmouth

sed -e s%@styr@%${yrst}% \
    -e s%@edyr@%${yred}% \
    namelist.extract_rivmouth_template > namelist.extract_rivmouth

make ${exe}
./${exe}
