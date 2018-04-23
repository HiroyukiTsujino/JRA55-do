#!/bin/bash -f

set -e

rm -f namelist.calibsat_ocn_ice

#for period in 2016_present
for period in 1958_1971 1972_1972 1973_1996 1997_1998 1999_2015 2016_present 1979_1996_jra55c
do
  ln -sf namelist.calibsat_ocn_ice_monthly_v7_${period} namelist.calibsat_ocn_ice
  ./calib_tmp2m_ocn_ice_interannual > calib_tmp2m_${period}.log
done
