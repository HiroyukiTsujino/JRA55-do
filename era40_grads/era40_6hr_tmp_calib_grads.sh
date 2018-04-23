#!/bin/bash

set -e

yearst=${1}
yeared=${2}

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
      namelist.era40_org2grads_tmpcalib_template > namelist.era40_org2grads_tmpcalib

  ./nc2grads_era40_tmp_calib_6hourly

  year=${yearn}

done
