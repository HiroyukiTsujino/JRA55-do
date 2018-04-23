#!/bin/bash -f

set -e

rm -f namelist.calibrad

for period in 1958_1971 1972_1972
do
  ln -sf namelist.calibrad.monthly_v7_1_${period} namelist.calibrad
  ./calib_rad_interannual > calib_rad_v7_1_${period}.log
done
