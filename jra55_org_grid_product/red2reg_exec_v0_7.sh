#!/bin/bash -f

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
  ./red2reg_exec_precip.sh  ${yearst} ${yeared} > precip.${yearst}-${yeared}.log
  ./red2reg_exec_rad.sh     ${yearst} ${yeared} > rad.${yearst}-${yeared}.log
  ./red2reg_exec_tmp_sph.sh ${yearst} ${yeared} > tmpsph.${yearst}-${yeared}.log
  ./red2reg_exec_wind.sh    ${yearst} ${yeared} > wind.${yearst}-${yeared}.log
else
  ./red2reg_exec_precip.sh  ${yearst} ${yeared} ${day_st} ${day_ed} > precip.${yearst}-${yeared}.log
  ./red2reg_exec_rad.sh     ${yearst} ${yeared} ${day_st} ${day_ed} > rad.${yearst}-${yeared}.log
  ./red2reg_exec_tmp_sph.sh ${yearst} ${yeared} ${day_st} ${day_ed} > tmpsph.${yearst}-${yeared}.log
  ./red2reg_exec_wind.sh    ${yearst} ${yeared} ${day_st} ${day_ed} > wind.${yearst}-${yeared}.log
fi
