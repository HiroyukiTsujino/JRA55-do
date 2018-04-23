#!/bin/bash -f

for item in dlwrf dswrf
do
  sed -e s%@item@%"${item}"% \
  namelist_org2daily_c2_template > namelist_org2daily
 ./make_org_to_daily
done
