#!/bin/bash -f

set -e

rm -f namelist.calibsph

for period in 2016_present
do
  ln -sf namelist.calibsph_monthly_v7_${period} namelist.calibsph
  ./calib_sph2m_interannual > calib_sph2m_${period}.log
done
