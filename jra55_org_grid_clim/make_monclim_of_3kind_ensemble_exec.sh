#!/bin/bash -f

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year"
  exit
fi

yearst=${1}
yeared=${2}

frac_valid_ice=0.66d0
frac_valid_ocn=0.66d0
frac_valid_all=1.0d0
out_latlon=.true.

sed -e s%@ibyr@%${yearst}% \
    -e s%@ieyr@%${yeared}% \
    -e s%@item@%${item}% \
    -e s%@out_latlon@%${out_latlon}% \
    -e s%@frac_valid_ice@%${frac_valid_ice}% \
    -e s%@frac_valid_ocn@%${frac_valid_ocn}% \
    -e s%@frac_valid_all@%${frac_valid_all}% \
    namelist.make_monclim_3kind_ensemble_tmp2m > namelist.make_monclim_3kind

./make_monclim_of_three_kind

rm -f namelist.make_monclim_3kind
