#!/bin/bash -f

set -e

ln -sf namelist.filterwind.jra55c namelist.filterwind

./filter_wind_interannual

rm -f namelist.filterwind
