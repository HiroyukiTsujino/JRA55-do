#!/bin/bash -f

set -e

for item in swind u10m v10m
do
  ln -sf namelist.make_monclim_fcst_e1_${item} namelist.make_monclim
 ./make_monclim
done

rm -f namelist.make_monclim
