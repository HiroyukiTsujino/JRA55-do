#!/bin/bash -f

#for item in slprs tmp10m sph10m dlwrf dswrf prcp evap
#for item in sph10m
for item in tmp10m
do
  sed -e s%@item@%"${item}"% \
  namelist_org2monthly_c1_template > namelist_org2monthly
 ./make_org_to_monthly
done
