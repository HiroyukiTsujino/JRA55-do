#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/CORE/core_cobesst_annual_dec2015/wvn10m
#file_base=../linkdir/CORE/core_cobesst_annual_jan2017/wvn10m
file_base=../linkdir/CORE/core_cobesst_annual_aug2017/wvn10m

l2d=.true.
cgrid=U

exe=have_ctl

for region in 60s60n
do
#fileo=wvn10m_${region}_jan2017
fileo=aug2017/wvn10m_${region}_aug2017
file_mask=/work116/htsujino/COBESST/data/${region}_mask.gd

./${exe}<<EOF
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=${l2d},
    cgrid="${cgrid}",
    file_mask="${file_mask}"
  /
EOF

done

exit 0
