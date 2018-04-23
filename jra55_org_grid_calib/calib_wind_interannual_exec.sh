#!/bin/bash -f

set -e

rm -f namelist.calibwind

#for period in 2016_present
#for period in 1958_1971
#for period in 1972_1972

for period in 1958_1971 1972_1972 1973_1996 1997_1998 1999_2015 2016_present
do
  ln -sf namelist.calibwind_v7_${period} namelist.calibwind
  ./calib_wind_interannual > calib_wind_${period}.log
done
