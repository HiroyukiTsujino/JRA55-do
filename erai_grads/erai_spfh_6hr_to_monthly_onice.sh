#!/bin/bash

set -e

yearst=${1}
yeared=${2}

frac_ratio=1.0d0

################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do

  yearn=`expr ${year} + 1`
  echo "year = ${year}  year_next = ${yearn}"

  sed -e s%@nyear@%${year}% \
      -e s%@nyear_next@%${yearn}% \
      -e s%@frac_ratio@%${frac_ratio}% \
      namelist.erai_spfh_org2mon_onice_template > namelist.erai_spfh_org2mon_onice

  ./nc2grads_erai_spfh_onice_6h2m

  year=${yearn}

done
