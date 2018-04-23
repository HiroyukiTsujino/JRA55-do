#!/bin/bash -f

yearst=${1}
yeared=${2}
creation_date=${3}
day_ed=${4}

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  leap=`isleap ${year}`
  num_day_year=`expr 365 + ${leap}`
  if [ x${day_ed} == x ]; then
    num_day=${num_day_year}
  else
    num_day=${day_ed}
  fi
  num_data=`expr ${num_day} * 8`
  echo "num_day = ${num_day}, num_data = ${num_data}"
  num_data_year=`expr ${num_day_year} * 8`
  echo "num_day_year = ${num_day_year}, num_data_year = ${num_data_year}"
  #for item in tmp10m sph10m u10m v10m slprs dswrf dlwrf precip # adjusted
  #for item in tmp10m sph10m u10m v10m dswrf dlwrf precip       # un-adjusted
  do
    sed -e s/@year@/${year}/ \
        -e s/@create@/${creation_date}/ \
        -e s/@num_data_year@/${num_data_year}/ \
        -e s/@num_data@/${num_data}/ \
        ${item}_nc.sh_template > ${item}_nc.sh
    chmod 755 ./${item}_nc.sh
    ./${item}_nc.sh
  done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
