#!/bin/bash -f

set -e

rm -f namelist.calibsat_ocn_ice

for period in 2016_present
do
  ln -sf namelist.calibsat_ocn_ice_monthly_v7_${period} namelist.calibsat_ocn_ice
  ./calib_tmp2m_ocn_ice_interannual > calib_tmp2m_${period}.log
done
