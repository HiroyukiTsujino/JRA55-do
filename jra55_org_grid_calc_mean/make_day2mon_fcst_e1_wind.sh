#!/bin/bash -f

set -e

if [ x${4} = x ]; then
  echo "Usage: ${0} start_year start_mon end_year end_mon"
  exit
fi

iyst=${1}
imst=${2}
iyed=${3}
imed=${4}

for item in swind u10m v10m
do
  sed -e s%@iyst@%${iyst}% \
      -e s%@imst@%${imst}% \
      -e s%@iyed@%${iyed}% \
      -e s%@imed@%${imed}% \
      -e s%@item@%${item}% \
  namelist_daily2monthly_fcst_e1_template > namelist_daily2monthly
 ./make_daily_to_monthly
done
