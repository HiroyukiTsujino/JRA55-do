#!/bin/bash

set -e

ln -sf NAMELIST.MXE.ERAI.annual_ocean NAMELIST.MXE

file_base=/work115/htsujino/ERA-interim/grads_annual/erai_wind10m
file_mask=/work115/htsujino/ERA-interim/data/tropics_mask.gd
fileo=wind10m_20S20N

l2d=.true.
cgrid=U

exe=have_ctl

./${exe}<<EOF
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=${l2d},
    file_mask="${file_mask}"
    cgrid="${cgrid}",
  /
EOF

exit 0
