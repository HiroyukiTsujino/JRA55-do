#!/bin/bash -f

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year (month day)"
  exit
fi

yearst=${1}
yeared=${2}

if [ x${3} = x ]; then
  nmon_last=12
else
  nmon_last=${3}
fi

if [ x${4} = x ]; then
  nday_last=-9
else
  nday_last=${4}
fi

l_solid_out=.true.
l_area_out=.false.

l_total_solid=.false.
l_liquid_solid=.true.

rm -f namelist.merge_main_grn_caa_ant

sed -e s%@ibyr@%${yearst}% \
    -e s%@ieyr@%${yeared}% \
    -e s%@nmon_last@%${nmon_last}% \
    -e s%@nday_last@%${nday_last}% \
    -e s%@l_solid_out@%${l_solid_out}% \
    -e s%@l_area_out@%${l_area_out}% \
    -e s%@l_total_solid@%${l_total_solid}% \
    -e s%@l_liquid_solid@%${l_liquid_solid}% \
    namelist.merge_main_grncaaB18_ant_template > namelist.merge_main_grn_caa_ant

target=merge_main_grn_caa_ant
make ${target}

./${target}
