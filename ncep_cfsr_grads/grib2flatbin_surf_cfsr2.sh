#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}

  i=1
  iend=12

  echo "year = ${year}  istart = ${i}  iend = ${iend}"

  while [ $i -le $iend ];
  do
    yr0=$( printf %04d $year )
    mn0=$( printf %02d $i )
    yyyymm=${yr0}${mn0}
    wgrib2 -match ":TMP:"  pgbh.gdas.${yyyymm}.grb2 -no_header -bin tmp2m.${yyyymm}
    wgrib2 -match ":SPFH:" pgbh.gdas.${yyyymm}.grb2 -no_header -bin sph2m.${yyyymm}
    i=$(( ${i} + 1 ))
  done

  year=$(( ${year} + 1 ))
done
