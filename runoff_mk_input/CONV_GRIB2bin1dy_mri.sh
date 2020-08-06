#!/bin/bash

set -e

if [ x${2} == x ]; then
   echo "Usage: ${0} start_year end_year (start_day end_day)"
   exit
fi

yrst=${1}
yred=${2}
dyst=${3}
dyed=${4}

#input_dir=../linkdir/forcing/fcst_phyland

input_dir=/mri-data/jra-55/Hist/Daily/fcst_phyland
newdir=../linkdir/work/jra55fcst_v1_3_input_runoff_3hr_TL319r

if [ ! -e ${newdir} ]; then
  echo "creating ${newdir}"
  mkdir ${newdir}
fi

################

. ../util/datel_leap.sh

year=${yrst}

while [ ${year} -le ${yred} ];
do
  echo ${year}
  leap=`isleap ${year}`
  if [ x${dyst} == x ]; then
    i=1
  else
    i=${dyst}
  fi
  if [ x${dyed} == x ]; then
    iend=`expr 365 + ${leap}`
  else
    iend=${dyed}
  fi

  echo "iend = ${iend}"

##################################################

  while [ ${i} -le ${iend} ];
  do
    yr=$( nydate $year $i | awk '{print $1 }' )
    mn=$( nydate $year $i | awk '{print $2 }' )
    dy=$( nydate $year $i | awk '{print $3 }' )

    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )
    dy0=$( printf %02d $dy )

    yyyymm=${yr0}${mn0}
    yyyymmdd=${yr0}${mn0}${dy0} 

    orgdir=${input_dir}/${yyyymm}
    newfile=${newdir}/watr${yyyymmdd}

    org_check=${orgdir}/fcst_phyland.${yyyymmdd}21

    if [ -e ${org_check} ]; then
      echo -n > $newfile
      echo $newfile
      for hh in 00 03 06 09 12 15 18 21
      do
        orgfile=${orgdir}/fcst_phyland.${yyyymmdd}${hh}
        echo $orgfile
        wgrib -s $orgfile | egrep '(:WATR:)' | wgrib -i -bin -ieee $orgfile  -append -o $newfile
      done
      i=$(( $i + 1 ))
    else
      i=$(( ${iend} + 1 ))
    fi

  done

  year=$(( ${year} + 1 ))

done
