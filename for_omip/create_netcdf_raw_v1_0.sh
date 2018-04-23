#!/bin/bash -f

set -e

if [ x${3} == x ]; then
   echo "Usage: ${0} start_year end_year creation_date day_end"
   exit
fi

rm -f NAMELIST.MXE
ln -s NAMELIST.MXE.JRA55 NAMELIST.MXE

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
#  for item in rain snow
#  for item in dswrf dlwrf
#  for item in u10m v10m
#  for item in tmp2m sph2m dswrf dlwrf
#  for item in wn10m
  do
    sed -e s/@year@/${year}/ \
        -e s/@create@/${creation_date}/ \
        -e s/@num_data_year@/${num_data_year}/ \
        -e s/@num_data@/${num_data}/ \
        ${item}_raw_v1_0_nc.sh_template > ${item}_nc.sh
    chmod 755 ./${item}_nc.sh
    ./${item}_nc.sh
  done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
