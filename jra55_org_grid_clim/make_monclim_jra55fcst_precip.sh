#!/bin/bash -f

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year"
  exit
fi

yearst=${1}
yeared=${2}

rm -f namelist.make_monclim

sed -e s%@ibyr@%${yearst}% \
    -e s%@ieyr@%${yeared}% \
    namelist.make_monclim_jra55fcst_precip_template > namelist.make_monclim

./make_monclim
