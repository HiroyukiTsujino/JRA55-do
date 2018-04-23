#!/bin/bash -f

set -e

#dsperiod=jra55_nov1999_oct2009
#dsperiod=jra55_jan1973_dec1996
#dsperiod=jra55_jan1988_dec1996

rm -f namelist.make_annclim_to_replace

ln -s namelist.make_annclim_to_replace_${dsperiod} namelist.make_annclim_to_replace

./make_annclim_to_replace
