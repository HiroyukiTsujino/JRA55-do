#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}
mon_st=${3}
mon_ed=${4}

remss_dir='/work116/htsujino/REMSS_WIND/grads_monthly'
anl_monthly='../linkdir/forcing/jra55anl_filt_monthly_TL319r'
blend_org='../linkdir/forcing/remss_jra55anl_filt_monthly_TL319r'
blend_latlon='../linkdir/forcing/remss_jra55anl_filt_monthly_TL319'
jra_cnst='../linkdir/forcing/jra_org/const'

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  if [ x${mon_st} == x ]; then
    i=1
  else
    i=${mon_st}
  fi
  if [ x${mon_ed} == x ]; then
    iend=12
  else
    iend=${mon_ed}
  fi

  echo "year = ${year}  istart = ${i}  iend = ${iend}"

  while [ $i -le $iend ];
  do
    yr=$year
    mn=$i
 
    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )

    yyyy=${yr0}
    yyyymm=${yr0}${mn0}

    sed -e s%@yyyy@%${yyyy}% \
        -e s%@yyyymm@%${yyyymm}% \
        -e s%@remss_dir@%${remss_dir}% \
        -e s%@anl_monthly@%${anl_monthly}% \
        -e s%@jra_cnst@%${jra_cnst}% \
        -e s%@blend_org@%${blend_org}% \
        -e s%@blend_latlon@%${blend_latlon}% \
	namelist.blend_remss_wind_anlmon_neutral_template > namelist.blend_remss_wind_anl_neutral

    ./blend_remss_wind_anl_neutral

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
