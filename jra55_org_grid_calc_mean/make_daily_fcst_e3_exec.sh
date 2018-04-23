#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}
day_st=${3}
day_ed=${4}

daily_dir='/work115/htsujino/SURF_FLUX/forcing/jra_daily_org_e3'
latlon_dir='/work115/htsujino/SURF_FLUX/forcing/jra_daily_latlon_e3'

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

  while [ $i -le $iend ];
  do
    yr=$( nydate $year $i | awk '{print $1 }' )
    mn=$( nydate $year $i | awk '{print $2 }' )
    dy=$( nydate $year $i | awk '{print $3 }' )
 
    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )
    dy0=$( printf %02d $dy )

    yyyymm=${yr0}${mn0}

    if [ ! -e ${daily_dir}/${yyyymm} ]; then
      echo "creating ${yyyymm}"
      mkdir ${daily_dir}/${yyyymm}
    fi

    if [ ! -e ${latlon_dir}/${yyyymm} ]; then
      echo "creating ${yyyymm}"
      mkdir ${latlon_dir}/${yyyymm}
    fi

    sed -e s/@year@/${yr}/ \
        -e s/@month@/${mn}/ \
        -e s/@day@/${dy}/ \
        namelist.make_daily_fcst_e3_template > namelist.make_daily_surf

    ./make_org_to_daily_surf

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
