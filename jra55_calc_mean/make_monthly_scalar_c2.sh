#!/bin/bash -f

for item in sph10m dlwrf dswrf prcp
do
  sed -e s%@item@%"${item}"% \
  namelist_org2monthly_c2_template > namelist_org2monthly
 ./make_org_to_monthly
done
