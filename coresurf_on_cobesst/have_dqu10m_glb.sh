#!/bin/bash

set -e 

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

file_base=/worke/htsujino/CORE/core_annual_cobe_dec2015/dqu10m
fileo=dqu10m_glb
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
