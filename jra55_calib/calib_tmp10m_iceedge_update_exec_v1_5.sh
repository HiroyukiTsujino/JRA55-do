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

orgdir=../linkdir/work/jra55fcst_v1_5_prod1_3hr_TL319
newdir=../linkdir/work/jra55fcst_v1_5_prod2_3hr_TL319

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

rm -f namelist.calibtmp10m_iceedge

sed -e s%@yrst@%${yrst}% \
    -e s%@mnst@%${mnst}% \
    -e s%@dyst@%${dyst}% \
    -e s%@yred@%${yred}% \
    -e s%@mned@%${mned}% \
    -e s%@dyed@%${dyed}% \
    namelist.calibtmp10m_iceedge_update_v1_5_template > namelist.calibtmp10m_iceedge

./calib_tmp10m_iceedge_mon_interannual > tmp10m_iceedge_update.log

rm -f namelist.calibtmp10m_iceedge
