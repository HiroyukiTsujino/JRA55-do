#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

basedir=../linkdir/forcing

#basedir=/work115/htsujino/SURF_FLUX/forcing
#basedir=/work116/htsujino/SURF_FLUX/forcing

#file_base=${basedir}/jra_cobesst_annual_c0/wind10m
#fileo=wind10m_glb_c0

#file_base=${basedir}/jra_cobesst_annual_c0_dec2015_2m/wind10m
#fileo=wind10m_glb_c0

#file_base=${basedir}/jra_cobesst_annual_e4/wind10m
#fileo=wind10m_glb_e4

#file_base=${basedir}/jra55fcst_annual_1x1/wvn10m
#fileo=v0_1/wvn10m_glb_v0_1

#file_base=${basedir}/jra55fcst_v1_1_annual_1x1/wvn10m
#fileo=wvn10m_glb_v1_1

#file_base=${basedir}/jra55fcst_v7_rad2_annual_1x1/wvn10m
#fileo=wvn10m_glb_v0_7_2

#file_base=${basedir}/jra55fcst_v1_4pre1_annual_1x1/wvn10m
#fileo=v1_4pre1/wvn10m_glb_v1_4pre1

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
