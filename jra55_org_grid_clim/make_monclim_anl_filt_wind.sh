#!/bin/bash -f

set -e

rm -f namelist.make_monclim

for item in wind10m u10m v10m
do
  sed -e s%@item@%${item}% \
    namelist.make_monclim_anl_filt_wind_template > namelist.make_monclim
 ./make_monclim
done
