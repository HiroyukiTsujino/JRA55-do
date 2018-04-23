#!/bin/bash -f

set -e

if [ x${2} = x ]; then
  echo "Usage: make_day2mon_fcst_jra55.sh item start_year start_month end_year end_month"
  exit
fi

item=${1}
iyst=${2}
imst=${3}
iyed=${4}
imed=${5}

#for item in tmp2m ice
#for item in tmp2m sph2m
#for item in tmp2m
#for item in sph2m
#do
sed -e s%@iyst@%${iyst}% \
    -e s%@imst@%${imst}% \
    -e s%@iyed@%${iyed}% \
    -e s%@imed@%${imed}% \
    -e s%@item@%${item}% \
namelist_daily2monthly_fcst_template > namelist_daily2monthly
./make_daily_to_monthly
#done
