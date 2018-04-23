#!/bin/bash -f

set -e

rm -f namelist.calibwind_anom_mag

#for period in 1958_1971
#for period in 1972_1972
#for period in 1958_1971 1973_1996 1997_1998 1999_2015 2016_present
#for period in 1958_1971 1972_1972 1973_1996 1997_1998 1999_2015 2016_present
#for period in 1999_2015
#for period in 1958_1971 1972_1972 1973_1996 1997_1998 1999_2016 2017_present
#for period in 1972_1972 1997_1998
#for period in 1958_1971 1973_1996
#for period in 1999_2015
for period in update
do
  ln -sf namelist.calibwind_anom_mag_v1_4_${period} namelist.calibwind_anom_mag
  ./calib_wind_anom_mag_interannual > calib_wind_v1_4_${period}.log
done
