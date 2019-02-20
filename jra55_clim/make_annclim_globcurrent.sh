#!/bin/bash -f

set -e

if [ -e namelist.annual_clim ];then
  rm -f namelist.annual_clim
fi

make mkannclim

for item in usurf vsurf
do
  ln -sf namelist.annual_clim_globcurrent_${item} namelist.annual_clim
 ./mkannclim
done

rm -f namelist.make_annclim
