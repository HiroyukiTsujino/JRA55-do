#!/bin/sh -f

item=${1}
yearst=${2}
yeared=${3}

mondir=../grdmon
newdir=../time_series

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  for mon in 01 02 03 04 05 06 07 08 09 10 11 12
  do
    cat ${mondir}/${item}.${year}${mon} >> ${newdir}/${item}.${year}
  done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
