#!/bin/bash -f

yearst=${1}
yeared=${2}

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  sh grib2flatbin_surf.sh  ${year} ${year}
  sh grib2flatbin_phy2m.sh ${year} ${year}
#  sh grib2flatbin_phy2m_wflx.sh ${year} ${year}
  sh grib2flatbin_phy2m_rain_snow.sh ${year} ${year}
  sh grib2flatbin_ice.sh   ${year} ${year}
  sh rgrid2latlon_exec.sh  ${year} ${year}
  sh shift_2m_to_10m_tq.sh ${year} ${year}
  yearn=`expr ${year} + 1`
  year=${yearn}
done
