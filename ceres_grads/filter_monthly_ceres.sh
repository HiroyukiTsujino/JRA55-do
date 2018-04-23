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

#basename_in=swdn_seaice_tropics
#basename_out=swdn_seaice_tropics_filter
basename_in=lwdn
basename_out=lwdn_filter

if [ ${edmon} -lt 10 ]; then
  edmon=0${edmon}
fi

for mon in `seq -w ${stmon} ${edmon}`
do
  yyyymm=${year}${mon}
  sed -e s%@yyyymm@%${yyyymm}% \
       -e s%@basename_in@%${basename_in}% \
       -e s%@basename_out@%${basename_out}% \
  namelist.filter_monthly_template > namelist.filter_clim
  ./filter_rad_clim
done

rm -f namelist.filter_clim
