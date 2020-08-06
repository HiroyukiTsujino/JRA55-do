#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}
day_st=${3}
day_ed=${4}

tmp2m=.false.
sph2m=.false.
wind=.false.
brtmp=.false.
slprs=.false.
ice=.true.

tmp2m_chk=.false.
sph2m_chk=.false.
wind_chk=.false.
brtmp_chk=.false.
slprs_chk=.false.
ice_chk=.false.

org_dir='../linkdir/forcing/jra_org'
daily_dir='../linkdir/forcing/jra_daily_org'
latlon_dir='../linkdir/forcing/jra_daily_latlon'

#org_dir='../linkdir/forcing/jra55fcst_v1_2_3hr_tmp2m_TL319r'
#org_dir='../linkdir/forcing/jra55fcst_v1_2_3hr_sph2m_TL319r'
#daily_dir='../linkdir/forcing/jra55fcst_v1_2_tq2m_daily_TL319r'
#latlon_dir='../linkdir/forcing/jra55fcst_v1_2_tq2m_daily_TL319'

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

    sed -e s%@year@%${yr}% \
        -e s%@month@%${mn}% \
        -e s%@day@%${dy}% \
        -e s%@org_dir@%${org_dir}% \
        -e s%@daily_dir@%${daily_dir}% \
        -e s%@latlon_dir@%${latlon_dir}% \
        -e s%@tmp2m@%${tmp2m}% \
        -e s%@sph2m@%${sph2m}% \
        -e s%@wind@%${wind}% \
        -e s%@brtmp@%${brtmp}% \
        -e s%@slprs@%${slprs}% \
        -e s%@ice@%${ice}% \
        -e s%@tmp2m_chk@%${tmp2m_chk}% \
        -e s%@sph2m_chk@%${sph2m_chk}% \
        -e s%@wind_chk@%${wind_chk}% \
        -e s%@brtmp_chk@%${brtmp_chk}% \
        -e s%@slprs_chk@%${slprs_chk}% \
        -e s%@ice_chk@%${ice_chk}% \
	namelist.make_daily_fcst_surf_template > namelist.make_daily_surf

    ./make_org_to_daily_surf

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
