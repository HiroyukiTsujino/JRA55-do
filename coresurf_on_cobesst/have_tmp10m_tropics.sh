#!/bin/bash

set -e

file_base=/worke/htsujino/CORE/core_monthly_cobe/tmp10m
fileo=tmp10m_20S20N
file_mask=/worke/htsujino/COBESST/data/tropics_mask.gd
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
