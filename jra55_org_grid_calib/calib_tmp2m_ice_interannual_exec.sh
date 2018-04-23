#!/bin/bash -f

set -e

dataset=jra55anl

rm -f namelist.calibsat_ice

ln -sf namelist.calibsat_ice_${dataset} namelist.calibsat_ice
./calib_tmp2m_ice_interannual > calib_tmp2m_ice_${dataset}.log

rm -f namelist.calibsat_ice
