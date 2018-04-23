#!/bin/bash

set -e

period=2001_2009

for item in dswrf dlwrf
do
  ln -sf namelist.monthly_clim_core_${item}_${period} namelist.monthly_clim
  ./mkmonclim
done

rm -f namelist.monthly_clim
