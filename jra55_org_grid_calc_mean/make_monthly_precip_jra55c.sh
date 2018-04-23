#!/bin/bash -f

set -e

iyst=1979
imst=1
iyed=2009
imed=12

rm -f namelist_org2monthly_flux

for item in prcp
do
  sed -e s%@iyst@%${iyst}% \
      -e s%@imst@%${imst}% \
      -e s%@iyed@%${iyed}% \
      -e s%@imed@%${imed}% \
      -e s%@item@%${item}% \
  namelist_org2monthly_jra55c_flux_org_template > namelist_org2monthly_flux
  ./make_org_to_monthly_flux
done
