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
#  ./red2reg_exec_precip_v1_3.sh  ${yearst} ${yeared} > precip_v1_3.${yearst}-${yeared}.log
#  ./red2reg_exec_rad_v1_3.sh     ${yearst} ${yeared} > rad_v1_3.${yearst}-${yeared}.log
  ./red2reg_exec_tmp_sph_v1_3.sh ${yearst} ${yeared} > tmpsph_v1_3.${yearst}-${yeared}.log
  ./red2reg_exec_wind_v1_3.sh    ${yearst} ${yeared} > wind_v1_3.${yearst}-${yeared}.log
else
#  ./red2reg_exec_precip_v1_3.sh  ${yearst} ${yeared} ${day_st} ${day_ed} > precip_v1_3.${yearst}-${yeared}.log
#  ./red2reg_exec_rad_v1_3.sh     ${yearst} ${yeared} ${day_st} ${day_ed} > rad_v1_3.${yearst}-${yeared}.log
  ./red2reg_exec_tmp_sph_v1_3.sh ${yearst} ${yeared} ${day_st} ${day_ed} > tmpsph_v1_3.${yearst}-${yeared}.log
  ./red2reg_exec_wind_v1_3.sh    ${yearst} ${yeared} ${day_st} ${day_ed} > wind_v1_3.${yearst}-${yeared}.log
fi
