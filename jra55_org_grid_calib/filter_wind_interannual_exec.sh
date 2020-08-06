#!/bin/bash

set -e

ln -sf namelist.filterwind.jra55 namelist.filterwind

./filter_wind_interannual

rm -f namelist.filterwind
