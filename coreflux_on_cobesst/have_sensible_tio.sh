#!/bin/bash

set -e

file_base=../linkdir/CORE/core_cobesst_annual_dec2015/sensible
fileo=sensible_tio_dec2015
#file_base=/worke/htsujino/CORE/core_annual_cobe_ly2009/sensible
#fileo=sensible_glb_ly2009

file_mask=/work116/htsujino/COBESST/data/tio_mask.gd
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
