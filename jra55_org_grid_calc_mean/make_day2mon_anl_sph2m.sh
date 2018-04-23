#!/bin/bash -f

set -e

iyst=2015
imst=1
iyed=2016
imed=1

for item in sph2m
do
  sed -e s%@iyst@%${iyst}% \
      -e s%@imst@%${imst}% \
      -e s%@iyed@%${iyed}% \
      -e s%@imed@%${imed}% \
      -e s%@item@%${item}% \
  namelist_daily2monthly_anl_template > namelist_daily2monthly
 ./make_daily_to_monthly
done
