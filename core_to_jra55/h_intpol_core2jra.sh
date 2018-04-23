#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}

year=${yearst}

orgdir="/work116/htsujino/CORE/core_T62_monthly"
newdir="../linkdir/forcing/core_monthly_TL319"

while [ ${year} -le ${yeared} ];
do
  for mm in 01 02 03 04 05 06 07 08 09 10 11 12
  do
#    for item in tmp10m_antarc
    for item in u10m v10m wind10m slprs tmp10m sph10m dlwrf dswrf prcp
    do
      flin1="${orgdir}/${item}.${year}${mm}"
      flot1="${newdir}/${item}.${year}${mm}"
      num_data=1
      echo ${flin1} ${flot1}
      sed -e s%@flin1@%"${flin1}"% \
          -e s%@flot1@%"${flot1}"% \
          -e s%@num_data@%${num_data}% \
          namelist_core2jra55_template > namelist_core2jra55
      ./core_to_jra
    done
  done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
