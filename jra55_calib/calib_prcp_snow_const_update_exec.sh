#!/bin/bash

set -e

if [ x${7} = x ]; then
  echo "Usage: ${0} version yrst mnst dyst yred mned dyed"
  exit
fi

ver=${1}
yrst=${2}
mnst=${3}
dyst=${4}
yred=${5}
mned=${6}
dyed=${7}

orgdir=../linkdir/work/jra55fcst_v1_2_prcp_3hr_TL319
newdir=../linkdir/work/jra55fcst_v1_3_prcp2_3hr_TL319

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

rm -f namelist.calibpcp

sed -e s%@yrst@%${yrst}% \
    -e s%@mnst@%${mnst}% \
    -e s%@dyst@%${dyst}% \
    -e s%@yred@%${yred}% \
    -e s%@mned@%${mned}% \
    -e s%@dyed@%${dyed}% \
    namelist.calibpcp.const_${ver}_update_template > namelist.calibpcp

./calib_precip_interannual > calib_prcp_const_${ver}_update.log

sed -e s%@yrst@%${yrst}% \
    -e s%@mnst@%${mnst}% \
    -e s%@dyst@%${dyst}% \
    -e s%@yred@%${yred}% \
    -e s%@mned@%${mned}% \
    -e s%@dyed@%${dyed}% \
    namelist.calibpcp.snow_const_${ver}_update_template > namelist.calibpcp
./calib_precip_interannual > calib_snow_const_${ver}_update.log

rm -f namelist.calibpcp