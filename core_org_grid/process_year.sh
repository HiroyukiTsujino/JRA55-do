#!/bin/bash -f

yearst=${1}
yeared=${2}

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  sed -e s/@year@/${year}/ \
      namelist_org2monthly_template > namelist_org2monthly
  ./monthly_ciaf
  yearn=`expr ${year} + 1`
  year=${yearn}
done
