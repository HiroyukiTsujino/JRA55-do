#!/bin/bash

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year"
  exit
fi

st_yr=${1}
ed_yr=${2}

exe=separated_runoff_noyrev

sed -e s/@styr@/${st_yr}/ \
    -e s/@edyr@/${ed_yr}/ \
namelist.separated_runoff_noyrev_jra55-do-v1_4_template > namelist.separated_runoff_noyrev

dir_out=jra55-do-v1_4-sep

if [ ! -e ${dir_out} ]; then
  mkdir -p ${dir_out}
fi

make ${exe}
./${exe}

exit 0
