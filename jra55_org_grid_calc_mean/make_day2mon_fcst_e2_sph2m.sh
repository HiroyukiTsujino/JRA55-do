#!/bin/bash -f

iyst=1979
imst=1
iyed=1998
imed=12

for item in sph2m
do
  sed -e s%@iyst@%${iyst}% \
      -e s%@imst@%${imst}% \
      -e s%@iyed@%${iyed}% \
      -e s%@imed@%${imed}% \
      -e s%@item@%${item}% \
  namelist_daily2monthly_fcst_e2_template > namelist_daily2monthly
 ./make_daily_to_monthly
done
