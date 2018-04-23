#!/bin/bash -f

set -e

rm -f namelist.calibrad

#for period in 1972_1972 2016_present
for period in 1958_1971 1973_2015
do
  ln -sf namelist.calibrad.monthly_v7_${period} namelist.calibrad
  ./calib_rad_interannual > calib_rad_${period}.log
done
