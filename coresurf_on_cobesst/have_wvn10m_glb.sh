#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/CORE/core_cobesst_annual_dec2015/wvn10m
#fileo=wvn10m_glb_dec2015

file_base=../linkdir/CORE/core_cobesst_annual_aug2017/wvn10m
fileo=aug2017/wvn10m_glb_aug2017


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

exit 0
