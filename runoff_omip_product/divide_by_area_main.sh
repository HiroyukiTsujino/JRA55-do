#!/bin/bash -f

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year (number_of_data)"
  exit
fi

yearst=${1}
yeared=${2}

l_area_out=.false.
file_area_out=dummy.gd

dir_in=/denkei-shared/og1/htsujino/SURF_FLUX/forcing/river_suzuki_v0_6
dir_out=/work116/htsujino/SURF_FLUX/forcing/jra55-do-v1_1-river

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  leap=`isleap ${year}`
  ndayyr=`expr 365 + ${leap}`

  echo "year = ${year}    ndayyr = ${ndayyr}"

  if [ x${3} = x ]; then
    num_data=${ndayyr}
  else
    num_data=${3}
  fi

  file_river_in=${dir_in}/runoff.${year}
  file_river_out=${dir_out}/runoff_CaMaFlood.${year}

  sed -e s%@file_river_in@%${file_river_in}% \
      -e s%@file_river_out@%${file_river_out}% \
      -e s%@num_data@%${num_data}% \
      -e s%@l_area_out@%${l_area_out}% \
      -e s%@file_area_out@%${file_area_out}% \
      namelist.divide_by_cellarea_template > namelist.divide_by_cellarea

  ./divide_by_cellarea

  yearn=`expr ${year} + 1`
  year=${yearn}

done
