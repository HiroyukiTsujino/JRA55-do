#!/bin/bash

set -e

ln -sf NAMELIST.MXE.JRA55ocean_annual NAMELIST.MXE

file_base=/work115/htsujino/SURF_FLUX/forcing/jra_anl_annual_latlon/tmp2m
fileo=tmp2m_20S20N
file_mask=../linkdir/data/tropics_mask.gd
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
