#!/bin/bash

set -e

ln -sf NAMELIST.MXE.JRA55_ocean_monthly_qscat NAMELIST.MXE

file_base=/work115/htsujino/SURF_FLUX/forcing/jra_qscat_monthly_latlon/swind_actual
fileo=swind_qscat_actual_60S60N
file_mask=../linkdir/data/satell_wind_mask.gd
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
