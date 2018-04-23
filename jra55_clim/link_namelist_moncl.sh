#!/bin/bash -f

ds_item=${1}
version=${2}

rm -f namelist.monthly_clim

ln -s namelist.monthly_clim_${ds_item}_${version} namelist.monthly_clim
