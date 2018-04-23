#!/bin/bash

set -e

yearst=${1}
yeared=${2}
var_name=${3}
var_name_out=${4}

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
      namelist.erai_org2mon_template > namelist.erai_org2mon

  ./nc2grads_erai_6h2m

  year=${yearn}

done
