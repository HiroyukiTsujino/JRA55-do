#!/bin/bash -f

set -e

#for item in u10m
#for item in v10m wind
for item in u10m v10m wind
do
  rm -f namelist_daily2monthly_miss
  sed -e s/@item@/${item}/ \
  namelist_daily2monthly_miss_qscat_strict_actual_free_template > namelist_daily2monthly_miss
  ./make_daily_to_monthly_with_missing
done
