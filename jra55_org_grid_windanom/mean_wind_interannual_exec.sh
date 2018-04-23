#!/bin/bash -f

set -e

#period=nov1999_oct2009_jra55_v1_2tq
#period=jan1988_dec1996_jra55_v1_2tq
#period=jan1988_dec1996_jra55C_v1_2tq

for period in nov1999_oct2009_jra55_v1_2tq jan1988_dec1996_jra55_v1_2tq jan1988_dec1996_jra55C_v1_2tq
do
  rm -f namelist.mean_wind_speed
  ln -sf namelist.mean_wind_speed_${period} namelist.mean_wind_speed
  ./mean_wind_speed_interannual > mean_wind_speed_${period}.log
done
