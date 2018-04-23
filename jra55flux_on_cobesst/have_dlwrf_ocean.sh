#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/forcing/jra_cobesst_annual_c0/dlwrf
#fileo=dlwrf_glb_c0

#file_base=../linkdir/forcing/jra_cobesst_annual_c2/dlwrf
#fileo=dlwrf_glb_c2

#file_base=../linkdir/forcing/jra_cobesst_annual_e3/dlwrf
#fileo=dlwrf_glb_e3

#file_base=../linkdir/forcing/jra_cobesst_annual_e4/dlwrf
#fileo=dlwrf_glb_e4

#file_base=../linkdir/forcing/jra55fcst_v7_rad1_annual_1x1/dlwrf
#fileo=dlwrf_glb_v0_7_1

#file_base=../linkdir/forcing/jra55fcst_v7_rad2_annual_1x1/dlwrf
#fileo=dlwrf_glb_v0_7_2

#file_base=../linkdir/forcing/jra55fcst_v7_bfadj_annual_1x1/dlwrf
#fileo=dlwrf_glb_v0_7_bfadj

#file_base=../linkdir/forcing/jra55fcst_annual_1x1/dlwrf
#fileo=v0_1/dlwrf_glb_v0_1

#file_base=../linkdir/forcing/jra55fcst_v1_2pre_annual_1x1/dlwrf
#fileo=v1_2pre/dlwrf_glb_v1_2pre

#file_base=../linkdir/forcing/jra55fcst_v1_2pre3_annual_1x1/dlwrf
#fileo=v1_2pre3/dlwrf_glb_v1_2pre3

#file_base=../linkdir/forcing/jra55fcst_v1_2_annual_1x1/dlwrf
#fileo=v1_2/dlwrf_glb_v1_2

#file_base=../linkdir/forcing/jra55fcst_v1_3pre1_annual_1x1/dlwrf
#fileo=v1_3pre1/dlwrf_glb_v1_3pre1

#file_base=../linkdir/forcing/jra55fcst_v1_3_annual_1x1/dlwrf
#fileo=v1_3/dlwrf_glb_v1_3

#file_base=../linkdir/forcing/jra55fcst_v1_4pre1_annual_1x1/dlwrf
#fileo=v1_4pre1/dlwrf_glb_v1_4pre1

#file_base=../linkdir/forcing/jra55fcst_v1_4_annual_1x1/dlwrf
#fileo=v1_4/dlwrf_glb_v1_4

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
