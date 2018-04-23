#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

file_base=../linkdir/CORE/core_cobesst_annual_aug2017/sensible
fileo=aug2017/sensible_glb_aug2017

#file_base=../linkdir/CORE/core_cobesst_annual_jan2017/sensible
#fileo=sensible_glb_jan2017

#file_base=../linkdir/CORE/core_cobesst_annual_dec2015/sensible
#fileo=sensible_glb_dec2015

#file_base=/worke/htsujino/CORE/core_annual_cobe_ly2009/sensible
#fileo=sensible_glb_ly2009

l2d=.true.
cgrid=U

exe=have_ctl

./${exe}<<EOF
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=${l2d},
    cgrid="${cgrid}",
  /
EOF

rm -f NAMELIST.MXE

exit 0
