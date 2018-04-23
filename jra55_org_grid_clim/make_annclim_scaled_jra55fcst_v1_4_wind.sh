#!/bin/bash -f

set -e

dsperiod=jra55_jan1988_dec1996

rm -f namelist.make_annclim_scaled

ln -s namelist.make_annclim_scaled_${dsperiod} namelist.make_annclim_scaled

./make_annclim_scaled
