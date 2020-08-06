#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}
mon_st=${3}
mon_ed=${4}

remss_jra='/work115/htsujino/SURF_FLUX/forcing/jra_remss_monthly_org'
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
    yr=${year}
    mn=${i}
    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )
    yyyy=${yr0}
    yyyymm=${yr0}${mn0}
#    for item in swind_actual swind_neutral
    for item in swind_actual
    do
      sed -e s%@yyyymm@%${yyyymm}% \
          -e s%@jra_cnst@%${jra_cnst}% \
          -e s%@qscat_jra@%${remss_jra}% \
          -e s%@item@%${item}% \
          namelist.zonalmean_on_jra55_template > namelist.zonalmean_on_jra55
      ./zonal_mean_on_jra55_grid
    done
    i=$(( $i + 1 ))
  done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
