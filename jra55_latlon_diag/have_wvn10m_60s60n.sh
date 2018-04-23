#!/bin/bash

set -e

ln -sf NAMELIST.MXE.JRA55_ocean_annual NAMELIST.MXE

# JRA-55 v0_1
#file_base=../linkdir/forcing/jra55fcst_annual_TL319/wvn10m
#fileo=wvn10m_v0_1_TL319_60S60N

# JRA-55 v0_7
#file_base=../linkdir/forcing/jra55fcst_v7_prod4_annual_TL319/wvn10m
#fileo=wvn10m_v0_7_TL319_60S60N

# JRA-55 v1_2
#file_base=../linkdir/forcing/jra55fcst_v1_2_prod4_annual_TL319/wvn10m
#fileo=wvn10m_v1_2_TL319_60S60N

# JRA-55 v1_3
#file_base=../linkdir/forcing/jra55fcst_v1_3_prod4_annual_TL319/wvn10m
#fileo=wvn10m_v1_3_TL319_60S60N

# JRA-55 v1_4
#file_base=../linkdir/forcing/jra55fcst_v1_4_prod4_annual_TL319/wvn10m
#fileo=wvn10m_v1_4_TL319_60S60N

file_mask=../linkdir/data/60s60n_mask.gd
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
