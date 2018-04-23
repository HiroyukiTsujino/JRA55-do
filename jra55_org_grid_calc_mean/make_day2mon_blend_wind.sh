#!/bin/bash -f

for item in swind u10m v10m
do
  ln -sf namelist_daily2monthly_blend_${item} namelist_daily2monthly
 ./make_daily_to_monthly
done
