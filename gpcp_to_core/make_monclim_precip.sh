#!/bin/bash -f

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year"
  exit
fi

yearst=${1}
yeared=${2}

#in_dir=/work116/htsujino/GPCP-v2_3/grads_monthly_T62
#out_dir=/work116/htsujino/GPCP-v2_3/grads_monclim_T62

#in_dir=/work116/htsujino/GPCP-v2_3/grads_monthly_adjusted_T62
#out_dir=/work116/htsujino/GPCP-v2_3/grads_monclim_adjusted_T62

rm -f ioinfmncl.dat

sed -e s%@ibyr@%${yearst}% \
    -e s%@ieyr@%${yeared}% \
    -e s%@in_dir@%${in_dir}% \
    -e s%@out_dir@%${out_dir}% \
    ioinfmncl.precip.dat_template > ioinfmncl.dat

./mkmonclim
