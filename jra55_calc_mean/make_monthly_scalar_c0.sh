#!/bin/bash -f

#for item in slprs tmp10m sph10m dlwrf dswrf prcp
for item in slprs sph10m dlwrf dswrf prcp evap
do
  sed -e s%@item@%"${item}"% \
  namelist_org2monthly_c0_template > namelist_org2monthly
 ./make_org_to_monthly
done
