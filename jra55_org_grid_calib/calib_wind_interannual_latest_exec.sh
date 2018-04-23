#!/bin/bash -f

set -e

rm -f namelist.calibwind

for period in 2016_present
do
  ln -sf namelist.calibwind_v7_${period} namelist.calibwind
  ./calib_wind_interannual > calib_wind_${period}.log
done
