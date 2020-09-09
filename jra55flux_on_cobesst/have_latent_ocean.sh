#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/forcing/jra_cobesst_annual_org/latent
#fileo=latent_glb_org
#file_base=../linkdir/forcing/jra_cobesst_annual_c0/latent
#fileo=latent_glb_c0
#file_base=../linkdir/forcing/jra_cobesst_annual_c0_dec2015_10m/latent
#fileo=latent_glb_c0_dec2015_10m
#file_base=../linkdir/forcing/jra_cobesst_annual_c0_dec2015_2m/latent
#fileo=latent_glb_c0_dec2015_2m
#file_base=../linkdir/forcing/jra_cobesst_annual_c2/latent
#fileo=latent_glb_c2_ly2009
#file_base=../linkdir/forcing/jra_cobesst_annual_d2/latent
#fileo=latent_glb_d2
#file_base=../linkdir/forcing/jra_cobesst_annual_d5/latent
#fileo=latent_glb_d5
#file_base=../linkdir/forcing/jra_cobesst_annual_d6/latent
#fileo=latent_glb_d6
#file_base=../linkdir/forcing/jra_cobesst_annual_e3/latent
#fileo=latent_glb_e3
#file_base=../linkdir/forcing/jra_cobesst_annual_e4/latent
#fileo=latent_glb_e4
#file_base=../linkdir/forcing/jra_cobesst_annual_e4_ly2009/latent
#fileo=latent_glb_e4_ly2009
#file_base=../linkdir/forcing/jra_cobesst_annual_e5/latent
#fileo=latent_glb_e5

#file_base=../linkdir/forcing/jra55fcst_v7_prod4_annual_1x1/latent
#fileo=latent_glb_v0_7

#file_base=../linkdir/forcing/jra55fcst_v7_rad1_annual_1x1/latent
#fileo=latent_glb_v0_7_1

#file_base=../linkdir/forcing/jra55fcst_v7_rad2_annual_1x1/latent
#fileo=latent_glb_v0_7_2

#file_base=../linkdir/forcing/jra55fcst_v7_bfadj_annual_1x1/latent
#fileo=latent_glb_v0_7_bfadj

#file_base=../linkdir/forcing/jra55fcst_annual_1x1/latent
#fileo=v0_1/latent_glb_v0_1

#file_base=../linkdir/forcing/jra55fcst_v1_1_annual_1x1/latent
#fileo=latent_glb_v1_1

#file_base=../linkdir/forcing/jra55fcst_v1_2pre_annual_1x1/latent
#fileo=v1_2pre/latent_glb_v1_2pre

#file_base=../linkdir/forcing/jra55fcst_v1_2pre3_annual_1x1/latent
#fileo=v1_2pre3/latent_glb_v1_2pre3

#file_base=../linkdir/forcing/jra55fcst_v1_2_annual_1x1/latent
#fileo=v1_2/latent_glb_v1_2

#file_base=../linkdir/forcing/jra55fcst_v1_3pre1_annual_1x1/latent
#fileo=v1_3pre1/latent_glb_v1_3pre1

#file_base=../linkdir/forcing/jra55fcst_v1_3_annual_1x1/latent
#fileo=v1_3/latent_glb_v1_3

#file_base=../linkdir/forcing/jra55fcst_v1_4pre1_annual_1x1/latent
#fileo=v1_4pre1/latent_glb_v1_4pre1

#file_base=../linkdir/forcing/jra55fcst_v1_4_annual_1x1/latent
#fileo=v1_4/latent_glb_v1_4

file_base=../linkdir/verification/jra55fcst_v1_3_01_annual_1x1/latent
fileo=v1_3_01/latent_glb_v1_3_01

#file_base=../linkdir/verification/jra55fcst_v1_5_annual_1x1/latent
#fileo=v1_5/latent_glb_v1_5


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
