#!/bin/bash -f

for item in blend fcst
do
  ln -sf namelist.make_monclim_on_ice_${item} namelist.make_monclim_on_ice
  ./make_monclim_on_ice
done

rm -f namelist.make_monclim_on_ice
