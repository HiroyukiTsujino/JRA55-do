#!/bin/bash

set -e

if [ x${4} == x ]; then
   echo "Usage: ${0} item start_year end_year end_day"
   exit
fi

item=${1}
yearst=${2}
yeared=${3}
dayed=${4}

##### ORGDIR #####
# MRI-CX2550
# latest (V1.5)
if [ ${item} = slprs -o ${item} = brtmp -o ${item} = ice ]; then
  orgdir=../linkdir/work/jra55fcst_3hr_TL319            # slprs brtmp ice
elif [ ${item} = tmp10m -o ${item} = sph10m ]; then
  orgdir=../linkdir/work/jra55fcst_v1_5_prod4_3hr_TL319 # tmp10m sph10m
elif [ ${item} = u10m -o ${item} = v10m ]; then
  orgdir=../linkdir/work/jra55fcst_v1_5_prod1_3hr_TL319 # u10m v10m
elif [ ${item} = dswrf -o ${item} = dlwrf ]; then
  orgdir=../linkdir/work/jra55fcst_v1_3_rad2_3hr_TL319  # dswrf dlwrf
elif [ ${item} = prcp -o ${item} = snow ]; then
  orgdir=../linkdir/work/jra55fcst_v1_3_prcp2_3hr_TL319 # prcp snow
else
  echo "orgdir for ${item} is not prepared "
  exit
fi

##### NEWDIR #####
# MRI-CX2550
# latest (V1.5)
if [ ${item} = brtmp -o ${item} = ice ]; then
  newdir=../linkdir/products/support/grads # brtmp ice
else
  newdir=../linkdir/products/version_1_5/grads # atmospheric variables
fi

################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}

  leap=`isleap ${year}`
  days_of_yr=`expr 365 + ${leap}`
  iend=${dayed}
  echo "iend = ${iend}"
  outfile=${item}.${year}

  if [ ! -e ${newdir}/${year} ]; then
    echo "creating ${year}"
    mkdir -p ${newdir}/${year}
  fi

  if [ -e ${newdir}/${year}/${outfile} ]; then
    echo "remove older file"
    rm -f ${newdir}/${year}/${outfile}
  fi

  touch ${newdir}/${year}/${outfile}
  echo "${newdir}/${year}/${outfile} created" 
  i=1
  while [ ${i} -le ${iend} ];
  do
    yr=$( nydate $year $i | awk '{print $1 }' )
    mn=$( nydate $year $i | awk '{print $2 }' )
    dy=$( nydate $year $i | awk '{print $3 }' )
   
    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )
    dy0=$( printf %02d $dy )
  
    yyyymm=${yr0}${mn0}
  
    date=${yr0}${mn0}${dy0} 
  
    for hh in 00 03 06 09 12 15 18 21
    do 
      echo ${i} ${date}${hh}
      cat ${orgdir}/${yyyymm}/${item}.${date}${hh} >> ${newdir}/${year}/${outfile}
    done
  
    i=$(( $i + 1 ))
  
  done

  cp ${orgdir}/${year}01/${item}.${year}010100 ${newdir}/${year}/${item}.${year}010100
  cp ${orgdir}/${year}01/${item}.${year}010103 ${newdir}/${year}/${item}.${year}010103

  if [ ${dayed} == ${days_of_yr} ]; then
    cp ${orgdir}/${year}12/${item}.${year}123121 ${newdir}/${year}/${item}.${year}123121
  fi

  yearn=`expr ${year} + 1`
  year=${yearn}

done
