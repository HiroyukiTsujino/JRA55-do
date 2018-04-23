#!/bin/bash -f

set -e

iyst=1972
imst=1
iyed=1998
imed=12

for item in swind u10m v10m
do
  sed -e s%@iyst@%${iyst}% \
      -e s%@imst@%${imst}% \
      -e s%@iyed@%${iyed}% \
      -e s%@imed@%${imed}% \
      -e s%@item@%${item}% \
  namelist_daily2monthly_jra55c_fcst_filt_template > namelist_daily2monthly
 ./make_daily_to_monthly
done
