#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/CORE/core_cobesst_annual_dec2015/evapor
#fileo=evapor_med_dec2015
#file_base=/worke/htsujino/CORE/core_annual_cobe_ly2009/evapor
#fileo=evapor_glb_ly2009

#file_base=../linkdir/CORE/core_cobesst_annual_jan2017/evapor
#fileo=evapor_med_jan2017

file_base=../linkdir/CORE/core_cobesst_annual_aug2017/evapor
fileo=aug2017/evapor_med_aug2017

file_mask=/work116/htsujino/COBESST/data/med_mask.gd
l2d=.true.
cgrid=U

exe=have_ctl

./${exe}<<EOF
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=${l2d},
    cgrid="${cgrid}",
    file_mask="${file_mask}"
  /
EOF

exit 0
