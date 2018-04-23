#!/bin/bash -f

set -e

rm -f namelist.make_annnclim

for item in wind10m u10m v10m
do
  sed -e s%@item@%${item}% \
    namelist.make_annclim_erai_wind_template > namelist.make_annclim
  ./make_annclim
done
