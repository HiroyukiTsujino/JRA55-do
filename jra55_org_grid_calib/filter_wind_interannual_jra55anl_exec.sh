#!/bin/bash -f

set -e

ln -sf namelist.filterwind.jra55anl namelist.filterwind

./filter_wind_interannual

rm -f namelist.filterwind
