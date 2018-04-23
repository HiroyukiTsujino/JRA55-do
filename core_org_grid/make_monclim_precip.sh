#!/bin/bash -f

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year"
  exit
fi

yearst=${1}
yeared=${2}

in_dir=/work116/htsujino/CORE/core_T62_monthly
out_dir=/work116/htsujino/CORE/core_T62_clim

rm -f ioinfmncl.dat

sed -e s%@ibyr@%${yearst}% \
    -e s%@ieyr@%${yeared}% \
    -e s%@in_dir@%${in_dir}% \
    -e s%@out_dir@%${out_dir}% \
    ioinfmncl.precip.dat_template > ioinfmncl.dat

./mkmonclim
