#!/bin/bash -f

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year"
  exit
fi

yearst=${1}
yeared=${2}

out_latlon=.true.

#

item=sph2m
frac_valid=1.0d0

sed -e s%@ibyr@%${yearst}% \
    -e s%@ieyr@%${yeared}% \
    -e s%@item@%${item}% \
    -e s%@out_latlon@%${out_latlon}% \
    -e s%@frac_valid@%${frac_valid}% \
    namelist.make_monclim_ensemble3_surf_template > namelist.make_monclim

./make_monclim
