#!/bin/bash -f

set -e

rm -f namelist.calibrad

for period in 2016_present
do
  ln -sf namelist.calibrad.monthly_v7_${period} namelist.calibrad
  ./calib_rad_interannual > calib_rad_${period}.log
done
