#!/bin/bash -f

set -e

ver=e3
for item in sph10m
do
  sed -e s%@item@%"${item}"% \
      -e s%@ver@%"${ver}"% \
  namelist_org2monthly_scalar_template > namelist_org2monthly_scalar
 ./make_org_to_monthly_scalar
done
