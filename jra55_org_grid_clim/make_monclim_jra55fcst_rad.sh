#!/bin/bash -f

set -e

for item in dswrf dlwrf
do
  ln -sf namelist.make_monclim_jra55fcst_${item} namelist.make_monclim
  ./make_monclim
done
