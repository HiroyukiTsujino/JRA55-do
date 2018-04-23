#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  sed -e s/@year@/${year}/ \
      namelist_org2monthly_tmpcutoff_template > namelist_org2monthly_tmpcutoff
  ./monthly_ciaf_tmpcutoff
  yearn=`expr ${year} + 1`
  year=${yearn}
done
