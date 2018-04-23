#!/bin/bash

set -e

yearst=${1}
yeared=${2}

################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do

  yearn=`expr ${year} + 1`
  echo "year = ${year}  year_next = ${yearn}"

  sed -e s%@nyear@%${year}% \
      -e s%@nyear_next@%${yearn}% \
      namelist.erai_wind_org2mon_template > namelist.erai_wind_org2mon

  ./nc2grads_erai_wind_6h2m

  year=${yearn}

done
