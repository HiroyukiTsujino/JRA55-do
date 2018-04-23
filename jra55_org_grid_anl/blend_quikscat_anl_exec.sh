#!/bin/bash -f

yearst=${1}
yeared=${2}
day_st=${3}
day_ed=${4}

qscat_dir='/work115/htsujino/QuikSCAT/qscat/bmaps_v04/grads_daily'
anl_daily='../linkdir/forcing/jra_anl_daily_org'
fcst_daily='../linkdir/forcing/jra_daily_org'
blend_org='../linkdir/forcing/jra_blend_daily_org'
blend_latlon='../linkdir/forcing/jra_blend_daily_latlon'
jra_cnst='../linkdir/forcing/jra_org/const'
cobe_dir='/work116/htsujino/COBESST/daily/grads'

out_latlon=.true.

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

  if [ ${year} -eq 1999 ]; then
    i=200
  fi
  if [ ${year} -eq 2009 ]; then
    iend=327
  fi

  echo "year = ${year}  istart = ${i}  iend = ${iend}"

  while [ $i -le $iend ];
  do
    yr=$( nydate $year $i | awk '{print $1 }' )
    mn=$( nydate $year $i | awk '{print $2 }' )
    dy=$( nydate $year $i | awk '{print $3 }' )
 
    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )
    dy0=$( printf %02d $dy )

    yyyy=${yr0}
    yyyymm=${yr0}${mn0}
    yyyymmdd=${yr0}${mn0}${dy0}

    if [ ! -e ${blend_org}/${yyyymm} ]; then
      echo "creating ${yyyymm}"
      mkdir ${blend_org}/${yyyymm}
    fi

    if [ ! -e ${blend_latlon}/${yyyymm} ]; then
      echo "creating ${yyyymm}"
      mkdir ${blend_latlon}/${yyyymm}
    fi

    sed -e s%@yyyy@%${yyyy}% \
        -e s%@yyyymm@%${yyyymm}% \
        -e s%@yyyymmdd@%${yyyymmdd}% \
        -e s%@qscat_dir@%${qscat_dir}% \
        -e s%@anl_daily@%${anl_daily}% \
        -e s%@fcst_daily@%${fcst_daily}% \
        -e s%@jra_cnst@%${jra_cnst}% \
        -e s%@blend_org@%${blend_org}% \
        -e s%@blend_latlon@%${blend_latlon}% \
        -e s%@cobe_dir@%${cobe_dir}% \
        -e s%@irec@%${i}% \
        -e s%@out_latlon@%${out_latlon}% \
	namelist.blend_quikscat_anl_template > namelist.blend_quikscat_anl

    ./blend_quikscat_anl

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
