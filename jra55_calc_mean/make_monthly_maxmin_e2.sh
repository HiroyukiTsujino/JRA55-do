#!/bin/bash -f

set -e

for item in tmp10m
do
  sed -e s%@item@%"${item}"% \
  namelist_org2monthly_max_min_e2_template > namelist_org2monthly_max_min
 ./make_org_to_monthly_max_min
done
