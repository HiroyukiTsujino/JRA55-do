#!/bin/bash

set -e

if [ x${6} == x ]; then
   echo "Usage: ${0} yrst mnst dyst yred mned dyed"
   exit
fi

yrst=${1}
mnst=${2}
dyst=${3}
yred=${4}
mned=${5}
dyed=${6}

orgdir=../linkdir/work/jra55fcst_filt_v1_2tq_3hr_TL319r
newdir=../linkdir/work/jra55fcst_filt_v1_3_3hr_wind_TL319r

year=${yrst}
while [ ${year} -le ${yred} ];
do
  for mon in `seq -f "%02g" 1 12`
  do
    if [ ! -e ${newdir}/${year}${mon} ]; then
      echo "creating ${year}${mon}"
      mkdir -p ${newdir}/${year}${mon}
    fi
  done
  year=`expr ${year} + 1`
done

rm -f namelist.calibwind_offset

sed -e s%@yrst@%${yrst}% \
    -e s%@mnst@%${mnst}% \
    -e s%@dyst@%${dyst}% \
    -e s%@yred@%${yred}% \
    -e s%@mned@%${mned}% \
    -e s%@dyed@%${dyed}% \
    namelist.calibwind_offset_v1_3_latest_template > namelist.calibwind_offset

./calib_wind_offset_interannual > calib_wind_v1_3_latest.log
