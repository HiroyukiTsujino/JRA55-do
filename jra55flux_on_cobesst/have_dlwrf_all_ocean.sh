#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/forcing/jra55fcst_annual_1x1/dlwrf_all
#fileo=v0_1/dlwrf_all_glb_v0_1

#file_base=../linkdir/forcing/jra55fcst_v1_1_annual_1x1/dlwrf_all
#fileo=dlwrf_all_glb_v1_1

#file_base=../linkdir/forcing/jra55fcst_v1_2_annual_1x1/dlwrf_all
#fileo=v1_2/dlwrf_all_glb_v1_2

#file_base=../linkdir/forcing/jra55fcst_v1_3_annual_1x1/dlwrf_all
#fileo=v1_3/dlwrf_all_glb_v1_3

#file_base=../linkdir/forcing/jra55Cfcst_annual_1x1/dlwrf_all
#fileo=dlwrf_all_glb_jra55c

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
