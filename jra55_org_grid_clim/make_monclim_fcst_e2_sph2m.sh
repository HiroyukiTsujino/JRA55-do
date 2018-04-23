#!/bin/bash -f

for item in sph2m
do
  ln -sf namelist.make_monclim_fcst_e2_${item} namelist.make_monclim
 ./make_monclim
done

rm -f namelist.make_monclim
