#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}
mon_st=${3}
mon_ed=${4}

remss_dir='/work116/htsujino/OAflux/grads_annual'
anl_monthly='/work115/htsujino/SURF_FLUX/forcing/jra_anl_monthly_org'
remss_jra='/work115/htsujino/SURF_FLUX/forcing/jra_remss_monthly_org'
jra_cnst='../linkdir/forcing/jra_org/const'

################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}

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
    yr0=$( printf %04d $year )
    mn0=$( printf %02d $i )

    yyyymm=${yr0}${mn0}

    sed -e s%@yyyymm@%${yyyymm}% \
        -e s%@remss_dir@%${remss_dir}% \
        -e s%@anl_monthly@%${anl_monthly}% \
        -e s%@jra_cnst@%${jra_cnst}% \
        -e s%@remss_jra@%${remss_jra}% \
        namelist.remsswind_on_jra55_template > namelist.remsswind_on_jra55

    ./remss_wind_on_jra55_grid

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
