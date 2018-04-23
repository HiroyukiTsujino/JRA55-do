#!/bin/bash -f

set -e

rm -f namelist.calibsph

#for period in 1999_2015 2016_present
#for period in 1972_1978_jra55c
#for period in 1958_1971 1972_1972 1973_1996 1997_1998 1999_2015 2016_present 1979_1996_jra55c
for period in 2016_present
do
  ln -sf namelist.calibsph_monthly_v1_2_${period} namelist.calibsph
  ./calib_sph2m_interannual > calib_v1_2_sph2m_${period}.log
done
