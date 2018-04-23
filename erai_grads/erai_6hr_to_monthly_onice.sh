#!/bin/bash

set -e

if [ x${4} = x ]; then
  echo "Usage: ${0} start_year end_year var_name_erai var_name_output"
  exit
fi

yearst=${1}
yeared=${2}
var_name=${3}
var_name_out=${4}

frac_ratio=1.0d0

################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do

  leap=`isleap ${year}`
  ndayyr=`expr 365 + ${leap}`

  yearn=`expr ${year} + 1`

  echo "year = ${year}  year_next = ${yearn}"

  sed -e s%@nyear@%${year}% \
      -e s%@nyear_next@%${yearn}% \
      -e s%@var_name@%${var_name}% \
      -e s%@var_name_out@%${var_name_out}% \
      -e s%@frac_ratio@%${frac_ratio}% \
      namelist.erai_org2mon_onice_template > namelist.erai_org2mon_onice

  ./nc2grads_erai_onice_6h2m

  year=${yearn}

done
