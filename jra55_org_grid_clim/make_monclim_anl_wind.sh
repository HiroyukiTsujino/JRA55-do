#!/bin/bash -f

for item in swind u10m v10m
do
  ln -sf namelist.make_monclim_anl_${item} namelist.make_monclim
 ./make_monclim
done
