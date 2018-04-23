#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/forcing/jra_cobesst_annual_org/evapor
#fileo=evapor_glb_org
#file_base=../linkdir/forcing/jra_cobesst_annual_c0/evapor
#fileo=evapor_glb_c0
#file_base=../linkdir/forcing/jra_cobesst_annual_c0_dec2015_10m/evapor
#fileo=evapor_glb_c0_dec2015_10m
#file_base=../linkdir/forcing/jra_cobesst_annual_c0_dec2015_2m/evapor
#fileo=evapor_glb_c0_dec2015_2m
#file_base=../linkdir/forcing/jra_cobesst_annual_c2/evapor
#fileo=evapor_glb_c2
#file_base=../linkdir/forcing/jra_cobesst_annual_e3/evapor
#fileo=evapor_glb_e3
#file_base=../linkdir/forcing/jra_cobesst_annual_e4/evapor
#fileo=evapor_glb_e4
#file_base=../linkdir/forcing/jra_cobesst_annual_e4_ly2009/evapor
#fileo=evapor_glb_e4_ly2009
#file_base=../linkdir/forcing/jra_cobesst_annual_e5/evapor
#fileo=evapor_glb_e5

#file_base=../linkdir/forcing/jra55fcst_v7_prod4_annual_1x1/evapor
#fileo=evapor_glb_v0_7

#file_base=../linkdir/forcing/jra55fcst_v7_rad1_annual_1x1/evapor
#fileo=evapor_glb_v0_7_1

#file_base=../linkdir/forcing/jra55fcst_v7_rad2_annual_1x1/evapor
#fileo=evapor_glb_v0_7_2

#file_base=../linkdir/forcing/jra55fcst_v7_bfadj_annual_1x1/evapor
#fileo=evapor_glb_v0_7_bfadj

#file_base=../linkdir/forcing/jra55fcst_annual_1x1/evapor
#fileo=v0_1/evapor_glb_v0_1

#file_base=../linkdir/forcing/jra55fcst_v1_1_annual_1x1/evapor
#fileo=v1_1/evapor_glb_v1_1

#file_base=../linkdir/forcing/jra55fcst_v1_2pre_annual_1x1/evapor
#fileo=v1_2pre/evapor_glb_v1_2pre

#file_base=../linkdir/forcing/jra55fcst_v1_2pre3_annual_1x1/evapor
#fileo=v1_2pre3/evapor_glb_v1_2pre3

#file_base=../linkdir/forcing/jra55fcst_v1_2_annual_1x1/evapor
#fileo=v1_2/evapor_glb_v1_2

#file_base=../linkdir/forcing/jra55fcst_v1_3pre1_annual_1x1/evapor
#fileo=v1_3pre1/evapor_glb_v1_3pre1

#file_base=../linkdir/forcing/jra55fcst_v1_3_annual_1x1/evapor
#fileo=v1_3/evapor_glb_v1_3

#file_base=../linkdir/forcing/jra55fcst_v1_4pre1_annual_1x1/evapor
#fileo=v1_4pre1/evapor_glb_v1_4pre1

#file_base=../linkdir/forcing/jra55fcst_v1_4_annual_1x1/evapor
#fileo=v1_4/evapor_glb_v1_4

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
