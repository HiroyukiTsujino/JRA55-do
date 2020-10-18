#!/bin/bash

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year (start_day end_day)"
  exit
fi

yearst=${1}
yeared=${2}
day_st=${3}
day_ed=${4}

year=${yearst}

if [ x${day_ed} = x ]; then
  ./red2reg_exec_tmp_sph_v1_5.sh ${yearst} ${yeared} > tmpsph_v1_5.${yearst}-${yeared}.log
  ./red2reg_exec_wind_v1_5.sh    ${yearst} ${yeared} > wind_v1_5.${yearst}-${yeared}.log
else
  ./red2reg_exec_tmp_sph_v1_5.sh ${yearst} ${yeared} ${day_st} ${day_ed} > tmpsph_v1_5.${yearst}-${yeared}.log
  ./red2reg_exec_wind_v1_5.sh    ${yearst} ${yeared} ${day_st} ${day_ed} > wind_v1_5.${yearst}-${yeared}.log
fi
