#!/bin/bash -f

set -e

for item in swind u10m v10m
do
  ln -sf namelist.make_annclim_blend_${item} namelist.make_annclim
 ./make_annclim
done
