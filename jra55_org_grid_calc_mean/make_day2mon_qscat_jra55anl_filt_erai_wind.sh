#!/bin/bash -f

set -e

if [ x${4} = x ]; then
  echo "Usage: make_day2mon_anl_wind.sh start_year start_month end_year end_month"
  exit
fi

iyst=${1}
imst=${2}
iyed=${3}
imed=${4}

for item in windn10m un10m vn10m
do
  sed -e s%@iyst@%${iyst}% \
      -e s%@imst@%${imst}% \
      -e s%@iyed@%${iyed}% \
      -e s%@imed@%${imed}% \
      -e s%@item@%${item}% \
  namelist_daily2monthly_qscat_jra55anl_filt_erai_template > namelist_daily2monthly
 ./make_daily_to_monthly
done
