#!/bin/bash -f

set -e

export OMP_NUM_THREADS=4

yearst=${1}
yeared=${2}
day_st=${3}
day_ed=${4}

jra_cnst='../linkdir/forcing/jra_org/const'
cobe_dir='/work116/htsujino/COBESST/daily/grads'

out_latlon=.true.
out_tmpsph=.true.
adjust_icesurf=.true.

# Input

fcst_org='../linkdir/forcing/jra55fcst_3hr_TL319r'
fcst_wind='../linkdir/forcing/jra55fcst_filt_test_3hr_TL319r'
fcst_tmp='../linkdir/forcing/jra55fcst_3hr_TL319r'
fcst_sph='../linkdir/forcing/jra55fcst_3hr_TL319r'

# Output

fcst_wind_out='../linkdir/forcing/jra55fcst_filt_test2_3hr_TL319r'
fcst_wind_latlon='../linkdir/forcing/jra55fcst_filt_test2_3hr_TL319'
fcst_tmp_out='../linkdir/forcing/jra55fcst_filt_test2_3hr_TL319r'
fcst_sph_out='../linkdir/forcing/jra55fcst_filt_test2_3hr_TL319r'

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
      mkdir ${fcst_wind_out}/${yyyymm}
    fi

    if [ ! -e ${fcst_wind_latlon}/${yyyymm} ]; then
      echo "creating ${yyyymm}"
      mkdir ${fcst_wind_latlon}/${yyyymm}
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
          -e s%@fcst_tmp_out@%${fcst_tmp_out}% \
          -e s%@fcst_sph_out@%${fcst_sph_out}% \
          -e s%@cobe_dir@%${cobe_dir}% \
          -e s%@yyyya@%${yyyya}% \
          -e s%@yyyyb@%${yyyyb}% \
          -e s%@ireca@%${ireca}% \
          -e s%@irecb@%${irecb}% \
          -e s%@hour@%${hour}% \
          -e s%@adjust_icesurf@%${adjust_icesurf}% \
          -e s%@out_tmpsph@%${out_tmpsph}% \
          -e s%@out_latlon@%${out_latlon}% \
      namelist.jra55_actual_wind_template > namelist.jra55_actual_wind

      ./diag_actual_wind

    done

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
