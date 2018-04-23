#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/forcing/jra55fcst_v7_rad1_annual_1x1/precip
#fileo=precip_med_v0_7_1

file_base=../linkdir/forcing/jra55fcst_v7_rad2_annual_1x1/precip
#fileo=precip_arctic_v0_7_2
#region_number=1
#fileo=precip_subpolatl_v0_7_2
#region_number=2
#fileo=precip_subarcatl_v0_7_2
#region_number=3

#file_base=../linkdir/forcing/jra55fcst_annual_1x1/precip
#fileo=precip_med_v1_1

file_mask=/work116/htsujino/COBESST/data/region_index.gd
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
    i_region_number=${region_number}
  /
EOF

exit 0
