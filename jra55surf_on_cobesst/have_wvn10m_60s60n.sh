#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

basedir=../linkdir/forcing

#file_base=${basedir}/jra55fcst_annual_1x1/wvn10m
#fileo=v0_1/wvn10m_60s60n_v0_1

#file_base=${basedir}/jra55fcst_v1_1_annual_1x1/wvn10m
#fileo=wvn10m_60s60n_v1_1

#file_base=${basedir}/jra55fcst_v1_2pre_annual_1x1/wvn10m
#fileo=wvn10m_60s60n_v1_2pre

#file_base=${basedir}/jra55fcst_v1_2_annual_1x1/wvn10m
#fileo=v1_2/wvn10m_60s60n_v1_2

#file_base=${basedir}/jra55fcst_v1_2pre3_annual_1x1/wvn10m
#fileo=v1_2pre3/wvn10m_60s60n_v1_2pre3

#file_base=${basedir}/jra55fcst_v1_3_annual_1x1/wvn10m
#fileo=v1_3/wvn10m_60s60n_v1_3

#file_base=${basedir}/jra55fcst_v1_4pre1_annual_1x1/wvn10m
#fileo=v1_4pre1/wvn10m_60s60n_v1_4pre1

#file_base=${basedir}/jra55fcst_v1_4_annual_1x1/wvn10m
#fileo=v1_4/wvn10m_60s60n_v1_4

file_mask=/work116/htsujino/COBESST/data/60s60n_mask.gd
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
