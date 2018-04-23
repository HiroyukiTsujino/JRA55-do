#!/bin/bash -f

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year"
  exit
fi

yearst=${1}
yeared=${2}

out_latlon=.true.

for item in dswrf dlwrf
do
  rm -f namelist.make_monclim
  sed -e s%@ibyr@%${yearst}% \
      -e s%@ieyr@%${yeared}% \
      -e s%@item@%${item}% \
      -e s%@out_latlon@%${out_latlon}% \
      namelist.make_monclim_jra55fcstv07_rad_template > namelist.make_monclim
  ./make_monclim
done
