#!/bin/bash -f

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year (end_month of the end_year)"
  exit
fi

yearst=${1}
yeared=${2}

if [ x${3} = x ]; then
  nmon_last=12
else
  nmon_last=${3}
fi

l_solid_out=.true.
l_area_out=.false.

rm -f namelist.merge_main_grn_ant

sed -e s%@ibyr@%${yearst}% \
    -e s%@ieyr@%${yeared}% \
    -e s%@nmon_last@%${nmon_last}% \
    -e s%@l_solid_out@%${l_solid_out}% \
    -e s%@l_area_out@%${l_area_out}% \
    namelist.merge_main_grnB18_ant_template > namelist.merge_main_grn_ant

target=merge_main_grn_ant
make ${target}

./${target}
