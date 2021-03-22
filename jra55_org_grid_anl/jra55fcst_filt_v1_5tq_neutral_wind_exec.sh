#!/bin/bash

set -e

#export OMP_NUM_THREADS=4

yearst=${1}
yeared=${2}
day_st=${3}
day_ed=${4}

jra_cnst='../linkdir/work/const'
cobe_dir='../linkdir/COBESST/daily/grads'

out_latlon=.false.
adjust_icesurf=.true.

fcst_org='../linkdir/work/jra55fcst_3hr_TL319r'
fcst_wind='../linkdir/work/jra55fcst_filt_3hr_TL319r'
fcst_tmp='../linkdir/work/jra55fcst_v1_5_3hr_tmp2m_TL319r'
fcst_sph='../linkdir/work/jra55fcst_v1_5_3hr_sph2m_TL319r'
fcst_wind_out='../linkdir/work/jra55fcst_filt_v1_5tq_3hr_TL319r'
fcst_wind_latlon='../linkdir/work/jra55fcst_filt_v1_5tq_3hr_TL319'

################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  leap=`isleap ${year}`
  ndayyr=`expr 365 + ${leap}`

  if [ x${day_st} == x ]; then
    i=1
  else
    i=${day_st}
  fi
  if [ x${day_ed} == x ]; then
    iend=${ndayyr}
  else
    iend=${day_ed}
  fi

  echo "year = ${year}  istart = ${i}  iend = ${iend}  ndayyr = ${ndayyr}"

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

    if [ ! -e ${fcst_wind_out}/${yyyymm} ]; then
      echo "creating ${yyyymm}"
      mkdir -p ${fcst_wind_out}/${yyyymm}
    fi

    if [ ${out_latlon} = .true. ]; then
      if [ ! -e ${fcst_wind_latlon}/${yyyymm} ]; then
        echo "creating ${yyyymm}"
        mkdir -p ${fcst_wind_latlon}/${yyyymm}
      fi
    fi

    for hour in 0 3 6 9 12 15 18 21
    do 
      if [ ${hour} -lt 12 ]; then
        if [ ${i} -eq 1 ]; then
          yyyyb=$(( ${year} - 1 ))
          leapb=`isleap ${yyyyb}`
          irecb=`expr 365 + ${leapb}`
          yyyya=${year}
          ireca=1
        else
          yyyyb=${year}
          irecb=$(( ${i} - 1 ))
          yyyya=${year}
          ireca=${i}
        fi
      else
        if [ ${i} -eq ${ndayyr} ]; then
          yyyyb=${year}
          irecb=${i}
          yyyya=$(( ${year} + 1 ))
          ireca=1
        else
          yyyyb=${year}
          irecb=${i}
          yyyya=${year}
          ireca=$(( ${i} + 1 ))
        fi
      fi
      hh=$( printf %02d ${hour} )
      yyyymmddhh=${yr0}${mn0}${dy0}${hh}

      sed -e s%@yyyy@%${yyyy}% \
          -e s%@yyyymm@%${yyyymm}% \
          -e s%@yyyymmddhh@%${yyyymmddhh}% \
          -e s%@jra_cnst@%${jra_cnst}% \
          -e s%@fcst_org@%${fcst_org}% \
          -e s%@fcst_wind@%${fcst_wind}% \
          -e s%@fcst_tmp@%${fcst_tmp}% \
          -e s%@fcst_sph@%${fcst_sph}% \
          -e s%@fcst_wind_out@%${fcst_wind_out}% \
          -e s%@fcst_wind_latlon@%${fcst_wind_latlon}% \
          -e s%@cobe_dir@%${cobe_dir}% \
          -e s%@yyyya@%${yyyya}% \
          -e s%@yyyyb@%${yyyyb}% \
          -e s%@ireca@%${ireca}% \
          -e s%@irecb@%${irecb}% \
          -e s%@hour@%${hour}% \
          -e s%@adjust_icesurf@%${adjust_icesurf}% \
          -e s%@out_latlon@%${out_latlon}% \
      namelist.jra55_neutral_wind_template > namelist.jra55_neutral_wind

      ./diag_neutral_wind

    done

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
