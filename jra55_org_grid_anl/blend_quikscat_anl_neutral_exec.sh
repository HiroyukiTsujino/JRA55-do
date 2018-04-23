#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}
day_st=${3}
day_ed=${4}

# 1999 1999 200 365
# 2009 2009   1 323

qscat_dir='/work115/htsujino/QuikSCAT/qscat/bmaps_v04/grads_daily_strict'
anl_daily='../linkdir/forcing/jra55anl_filt_erai_daily_TL319r'
blend_org='../linkdir/forcing/qscat_jra55anl_filt_erai_daily_TL319r'
blend_latlon='../linkdir/forcing/qscat_jra55anl_filt_erai_daily_TL319'
jra_cnst='../linkdir/forcing/jra_org/const'

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
    iend=323
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
        -e s%@jra_cnst@%${jra_cnst}% \
        -e s%@blend_org@%${blend_org}% \
        -e s%@blend_latlon@%${blend_latlon}% \
        -e s%@out_latlon@%${out_latlon}% \
	namelist.blend_quikscat_anl_neutral_template > namelist.blend_quikscat_anl_neutral

    ./blend_quikscat_anl_neutral

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
