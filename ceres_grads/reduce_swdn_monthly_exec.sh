#!/bin/bash

set -e

if [ x${3} = x ]; then
  echo "Usage ${0} year start_mon end_mon"
  exit
fi

ln -sf NAMELIST.MXE.CERES.monthly NAMELIST.MXE

year=${1}
stmon=${2}
edmon=${3}

basename_in=swdn_reduced_over_seaice
basename_out=swdn_seaice_tropics

if [ ${edmon} -lt 10 ]; then
  edmon=0${edmon}
fi

for mon in `seq -w ${stmon} ${edmon}`
do
  yyyymm=${year}${mon}
  sed -e s%@yyyymm@%${yyyymm}% \
      -e s%@basename_in@%${basename_in}% \
      -e s%@basename_out@%${basename_out}% \
    namelist.reduce_ceres_monthly_swdn_template > namelist.reduce_ceres_swdn
  ./reduce_swdn_clim
done
