#!/bin/bash

set -e

if [ x${2} == x ]; then
   echo "Usage: ${0} start_year end_year"
   exit
fi

yearst=${1}
yeared=${2}

################

# V1.5 (slprs, u10m, v10m, tmp10m, sph10m, dswrf, dlwrf, prcp, snow)
orgdir=/work115/htsujino/SURF_FLUX/forcing/jra55fcst_v1_5_3hr_TL319
newdir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom_v1_5/TL319_grid

################

. ../util/datel_leap.sh

for item in slprs u10m v10m tmp10m sph10m dswrf dlwrf prcp snow
do

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  leap=`isleap ${year}`
  iend=`expr 365 + ${leap}`
  echo "iend = ${iend}"
  outfile=${item}.${year}

  if [ ! -e ${newdir}/${year} ]; then
    echo "creating ${year}"
    mkdir ${newdir}/${year}
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
      #echo ${i} ${date}${hh}
      cat ${orgdir}/${yyyymm}/${item}.${date}${hh} >> ${newdir}/${year}/${outfile}
    done

    i=$(( $i + 1 ))

  done

  cp ${orgdir}/${year}01/${item}.${year}010100 ${newdir}/${year}/${item}.${year}010100
  cp ${orgdir}/${year}01/${item}.${year}010103 ${newdir}/${year}/${item}.${year}010103
  cp ${orgdir}/${year}12/${item}.${year}123121 ${newdir}/${year}/${item}.${year}123121

  yearn=`expr ${year} + 1`
  year=${yearn}

done

done
