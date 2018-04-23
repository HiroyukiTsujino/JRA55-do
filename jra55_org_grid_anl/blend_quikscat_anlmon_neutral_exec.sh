#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}
mon_st=${3}
mon_ed=${4}

qscat_dir='/work115/htsujino/QuikSCAT/qscat/bmaps_v04/grads_monthly_strict'
anl_monthly='../linkdir/forcing/jra55anl_filt_monthly_TL319r'
blend_org='../linkdir/forcing/qscat_jra55anl_filt_monthly_TL319r'
blend_latlon='../linkdir/forcing/qscat_jra55anl_filt_monthly_TL319'
jra_cnst='../linkdir/forcing/jra_org/const'

out_latlon=.true.

################

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

  if [ ${year} -eq 1999 ]; then
    i=8
  fi
  if [ ${year} -eq 2009 ]; then
    iend=10
  fi

  echo "year = ${year}  istart = ${i}  iend = ${iend}"

  while [ $i -le $iend ];
  do
    yr0=$( printf %04d $year )
    mn0=$( printf %02d $i )

    yyyy=${yr0}
    yyyymm=${yr0}${mn0}

    sed -e s%@yyyy@%${yyyy}% \
        -e s%@yyyymm@%${yyyymm}% \
        -e s%@qscat_dir@%${qscat_dir}% \
        -e s%@anl_monthly@%${anl_monthly}% \
        -e s%@jra_cnst@%${jra_cnst}% \
        -e s%@blend_org@%${blend_org}% \
        -e s%@blend_latlon@%${blend_latlon}% \
        -e s%@out_latlon@%${out_latlon}% \
	namelist.blend_quikscat_anlmon_neutral_template > namelist.blend_quikscat_anl_neutral

    ./blend_quikscat_anl_neutral

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
