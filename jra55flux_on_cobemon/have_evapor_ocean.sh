#!/bin/bash

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

file_base=../linkdir/forcing/jra_cobemon_annual_c2/evapor
fileo=evapor_glb_c2

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
