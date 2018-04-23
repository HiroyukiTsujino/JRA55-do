#!/bin/bash -f

set -e

rm -f namelist.make_annclim

for item in wind10m u10m v10m
do
  sed -e s%@item@%${item}% \
    namelist.make_annclim_anl_filt_wind_template > namelist.make_annclim
  ./make_annclim
done
