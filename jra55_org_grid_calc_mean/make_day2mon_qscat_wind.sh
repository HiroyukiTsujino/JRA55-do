#!/bin/bash -f

set -e

for item in actual neutral
do
#  ln -sf namelist_daily2monthly_miss_qscat_${item}_swind namelist_daily2monthly_miss
  ln -sf namelist_daily2monthly_miss_qscat_strict_${item}_swind namelist_daily2monthly_miss
 ./make_daily_to_monthly_with_missing
done
