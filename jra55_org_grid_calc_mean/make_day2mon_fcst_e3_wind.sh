#!/bin/bash -f

iyst=1999
imst=1
iyed=2009
imed=12

for item in swind u10m v10m
do
  sed -e s%@iyst@%${iyst}% \
      -e s%@imst@%${imst}% \
      -e s%@iyed@%${iyed}% \
      -e s%@imed@%${imed}% \
      -e s%@item@%${item}% \
  namelist_daily2monthly_fcst_e3_template > namelist_daily2monthly
 ./make_daily_to_monthly
done
