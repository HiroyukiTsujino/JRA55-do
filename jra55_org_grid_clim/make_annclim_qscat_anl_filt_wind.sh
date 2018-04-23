#!/bin/bash -f

set -e

rm -f namelist.make_annclim

for item in wn10m un10m vn10m
do
  sed -e s%@item@%${item}% \
    namelist.make_annclim_qscat_anl_filt_wind_template > namelist.make_annclim
  ./make_annclim
done
