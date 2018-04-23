#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/forcing/jra55fcst_v7_rad1_annual_1x1/precip
#fileo=precip_med_v0_7_1

#file_base=../linkdir/forcing/jra55fcst_v7_rad2_annual_1x1/precip
#fileo=precip_med_v0_7_2

#file_base=../linkdir/forcing/jra55fcst_v7_bfadj_annual_1x1/precip
#fileo=precip_med_v0_7_bfadj

#file_base=../linkdir/forcing/jra55fcst_annual_1x1/precip
#fileo=precip_med_v0_1

#file_base=../linkdir/forcing/jra55fcst_v1_2_prcp0_annual_1x1/precip
#fileo=v1_2_prcp0/precip_med_v1_2_prcp0

#file_base=../linkdir/forcing/jra55fcst_v1_2_prcp_annual_1x1/precip
#fileo=v1_2_prcp/precip_med_v1_2_prcp

#file_base=../linkdir/forcing/jra55fcst_v1_2_annual_1x1/precip
#fileo=v1_2/precip_med_v1_2

file_mask=/work116/htsujino/COBESST/data/med_mask.gd
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
