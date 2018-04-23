#!/bin/bash -f

yearst=${1}
yeared=${2}
day_st=${3}
day_ed=${4}

newdir=../linkdir/forcing/jra_latlon

################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}

  yearb=$(( ${year} - 1 ))
  if [ ${yearb} -eq 1957 ]; then
    yyyymmb=195801
    yyyymmddhhb=1958010100
  else
    yyyymmb=${yearb}12
    yyyymmddhhb=${yearb}123121
  fi

  leap=`isleap ${year}`
  if [ x${day_st} == x ]; then
    i=1
  else
    i=${day_st}
    ib=`expr ${i} - 1`
    yr=$( nydate $year $ib | awk '{print $1 }' )
    mn=$( nydate $year $ib | awk '{print $2 }' )
    dy=$( nydate $year $ib | awk '{print $3 }' )
    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )
    dy0=$( printf %02d $dy )
    yyyymmb=${yr0}${mn0}
    date=${yr0}${mn0}${dy0} 
    yyyymmddhhb=${date}21
  fi
  if [ x${day_ed} == x ]; then
    iend=`expr 365 + ${leap}`
  else
    iend=${day_ed}
  fi

  echo "iend = ${iend}"

  while [ $i -le $iend ];
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
      yyyymmddhh=${date}${hh}
      yyyymma=${yyyymm}
      yyyymmddhha=${yyyymmddhh}
      echo ${i} ${yyyymmddhh} ${yyyymmddhhb} ${yyyymmddhha}
      sed -e s/@yyyymmddhh@/${yyyymmddhh}/ \
          -e s/@yyyymmddhha@/${yyyymmddhha}/ \
          -e s/@yyyymmddhhb@/${yyyymmddhhb}/ \
          -e s/@yyyymm@/${yyyymm}/ \
          -e s/@yyyymma@/${yyyymma}/ \
          -e s/@yyyymmb@/${yyyymmb}/ \
          namelist.shift_height_zrough_template > namelist.shift_height_zrough
      ./shift_2m_to_10m_zrough
      yyyymmb=${yyyymma}
      yyyymmddhhb=${yyyymmddhha}
    done

    i=$(( $i + 1 ))

  done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
