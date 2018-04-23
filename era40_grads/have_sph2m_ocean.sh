#!/bin/bash

set -e

ln -sf NAMELIST.MXE.ERA40.annual_ocean NAMELIST.MXE

file_base=/work115/htsujino/ERA-40/grads_annual/era40_sph2m
fileo=sph2m_glb

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
