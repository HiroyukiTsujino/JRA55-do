#!/bin/bash

set -e

ln -sf NAMELIST.MXE.HurrellSST.ocean_annual NAMELIST.MXE

file_base=../linkdir/forcing/jra55fcst_v1_3_annual_hurrellsst_1x1/nswrf
fileo=v1_3/nswrf_glb_v1_3_hurrell

l2d=.true.
cgrid=U

exe=have_ctl

./${exe}<<EOF
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=${l2d},
    cgrid="${cgrid}",
    l_area_write=.false.
  /
EOF

exit 0
