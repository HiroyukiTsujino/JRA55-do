#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}
day_st=${3}
day_ed=${4}

orgdir=../linkdir/forcing/ice
newdir=../linkdir/forcing/jra_org

################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  leap=`isleap ${year}`
  if [ x${day_st} == x ]; then
    i=1
  else
    i=${day_st}
  fi
  if [ x${day_ed} == x ]; then
    iend=`expr 365 + ${leap}`
  else
    iend=${day_ed}
  fi

  echo "iend = ${iend}"

  while [ ${i} -le ${iend} ];
  do
    yr=$( nydate $year $i | awk '{print $1 }' )
    mn=$( nydate $year $i | awk '{print $2 }' )
    dy=$( nydate $year $i | awk '{print $3 }' )
 
    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )
    dy0=$( printf %02d $dy )

    yyyymm=${yr0}${mn0}

    if [ ! -e ${newdir}/${yyyymm} ]; then
      echo "creating ${yyyymm}"
      mkdir ${newdir}/${yyyymm}
    fi

    date=${yr0}${mn0}${dy0} 

    for hh in 00 03 06 09 12 15 18 21
    do 
      echo ${i} ${date}${hh}
      wgrib -s ${orgdir}/${yyyymm}/ice.${date}${hh} | egrep '(:ICEC:sfc:)' | wgrib -i -bin -nh ${orgdir}/${yyyymm}/ice.${date}${hh} -o ${newdir}/${yyyymm}/ice.${date}${hh}
    done

    i=$(( $i + 1 ))

  done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
