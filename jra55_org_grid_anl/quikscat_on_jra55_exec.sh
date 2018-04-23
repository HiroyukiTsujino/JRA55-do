#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}
day_st=${3}
day_ed=${4}

#qscat_dir='/work115/htsujino/QuikSCAT/qscat/bmaps_v04/grads_daily'
qscat_dir='/work115/htsujino/QuikSCAT/qscat/bmaps_v04/grads_daily_strict'
anl_daily='/work115/htsujino/SURF_FLUX/forcing/jra_anl_daily_org'
fcst_daily='../linkdir/forcing/jra_daily_org'
#qscat_jra='/work115/htsujino/SURF_FLUX/forcing/jra_qscat_daily_org'
qscat_jra='/work115/htsujino/SURF_FLUX/forcing/jra_qscat_strict_daily_org'
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

    if [ ! -e ${qscat_jra}/${yyyymm} ]; then
      echo "creating ${yyyymm}"
      mkdir ${qscat_jra}/${yyyymm}
    fi

    sed -e s%@yyyy@%${yyyy}% \
        -e s%@yyyymm@%${yyyymm}% \
        -e s%@yyyymmdd@%${yyyymmdd}% \
        -e s%@qscat_dir@%${qscat_dir}% \
        -e s%@anl_daily@%${anl_daily}% \
        -e s%@fcst_daily@%${fcst_daily}% \
        -e s%@jra_cnst@%${jra_cnst}% \
        -e s%@qscat_jra@%${qscat_jra}% \
        -e s%@cobe_dir@%${cobe_dir}% \
        -e s%@irec@%${i}% \
	namelist.quikscat_on_jra55_template > namelist.quikscat_on_jra55

    ./quikscat_on_jra55_grid

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
