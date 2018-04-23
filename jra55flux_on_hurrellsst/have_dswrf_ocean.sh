#!/bin/bash

set -e

ln -sf NAMELIST.MXE.HurrellSST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/forcing/jra_hurrellsst_annual_c2/dswrf
#fileo=dswrf_glb_c2
file_base=/work115/htsujino/SURF_FLUX/forcing/jra_hurrellsst_annual_e4/dswrf
fileo=dswrf_glb_e4

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
