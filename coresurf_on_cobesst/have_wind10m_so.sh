#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

file_base=/worke/htsujino/CORE/core_annual_cobe_dec2015/wind10m
fileo=wind10m_80S30S
file_mask=/worke/htsujino/COBESST/data/so_mask.gd

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
