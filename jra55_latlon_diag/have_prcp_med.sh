#!/bin/bash

set -e

ln -sf NAMELIST.MXE.JRA55_ocean_annual NAMELIST.MXE
#file_base=../linkdir/forcing/jra55fcst_v7_prcp_annual_TL319/prcp
#fileo=prcp_med_jra55_v0_7_1958-2015
#file_base=../linkdir/forcing/jra55fcst_v7_prcp3_annual_TL319/prcp
#fileo=prcp_med_jra55_v0_7_3_1958-2015

#ln -sf NAMELIST.MXE.CORE_ocean_annual NAMELIST.MXE
#file_base=../linkdir/forcing/core_annual_TL319/prcp
#fileo=prcp_med_core_1958-2015

file_mask=../linkdir/data/med_mask.gd
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
