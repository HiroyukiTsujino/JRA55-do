#!/bin/bash

ln -sf NAMELIST.NOCS.monthly NAMELIST.NOCS

file_base=/worke/htsujino/NOCS/grads/qair
fileo=sph10m_20S20N
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
