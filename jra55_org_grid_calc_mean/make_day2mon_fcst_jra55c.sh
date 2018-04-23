#!/bin/bash -f

set -e

iyst=1979
imst=1
iyed=2012
imed=12

#for item in tmp2m sph2m
#for item in tmp2m
for item in sph2m
do
  sed -e s%@iyst@%${iyst}% \
      -e s%@imst@%${imst}% \
      -e s%@iyed@%${iyed}% \
      -e s%@imed@%${imed}% \
      -e s%@item@%${item}% \
  namelist_daily2monthly_jra55c_fcst_template > namelist_daily2monthly
 ./make_daily_to_monthly
done
