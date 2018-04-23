#!/bin/bash

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

file_base=../linkdir/forcing/jra_cobesst_annual_c0/tmp10m
fileo=tmp10m_20S20N
file_mask=/work116/htsujino/COBESST/data/tropics_mask.gd
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
