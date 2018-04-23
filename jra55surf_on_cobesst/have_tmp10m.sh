#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE
#ln -sf NAMELIST.MXE.COBESST.ocean_annual_v0_2 NAMELIST.MXE

basedir=../linkdir/forcing
#basedir=/work115/htsujino/SURF_FLUX/forcing

#file_base=${basedir}/jra_cobesst_annual_c0_jul2015/tmp10m
#fileo=tmp10m_glb_c0_jul2015

#file_base=${basedir}/jra_cobesst_annual_c2/tmp10m
#fileo=tmp10m_glb_c2

#file_base=${basedir}/jra55fcst_annual_1x1/tmp10m
#fileo=v0_1/tmp10m_glb_v0_1

#file_base=${basedir}/jra55fcst_v7_rad2_annual_1x1/tmp10m
#fileo=tmp10m_glb_v0_7_2

#file_base=${basedir}/jra55fcst_v1_1_annual_1x1/tmp10m
#fileo=tmp10m_glb_v1_1

#file_base=${basedir}/jra55fcst_v1_2_annual_1x1/tmp10m
#fileo=v1_2/tmp10m_glb_v1_2

#file_base=${basedir}/jra55fcst_v1_2pre3_annual_1x1/tmp10m
#fileo=tmp10m_glb_v1_2pre3

#file_base=${basedir}/jra55fcst_v1_3_annual_1x1/tmp10m
#fileo=v1_3/tmp10m_glb_v1_3

#file_base=${basedir}/jra55fcst_v1_4pre1_annual_1x1/tmp10m
#fileo=v1_4pre1/tmp10m_glb_v1_4pre1

#file_base=${basedir}/jra55fcst_v1_4_annual_1x1/tmp10m
#fileo=v1_4/tmp10m_glb_v1_4

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
