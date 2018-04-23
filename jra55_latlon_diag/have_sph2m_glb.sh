#!/bin/bash

set -e

#ln -sf NAMELIST.MXE.JRA55_ocean_annual NAMELIST.MXE
#ln -sf NAMELIST.MXE.JRA55C_ocean_annual NAMELIST.MXE
#ln -sf NAMELIST.MXE.CORE_ocean_annual NAMELIST.MXE
#ln -sf NAMELIST.MXE.ensemble_ocean_annual NAMELIST.MXE

# JRA-55 fcst
#file_base=../linkdir/forcing/jra_annual_latlon/sph2m
# JRA-55 anl
#file_base=../linkdir/forcing/jra_anl_annual_latlon/sph2m
# JRA-55C 
#file_base=../linkdir/forcing/jra55c_annual_latlon/sph2m
# CORE
#file_base=../linkdir/forcing/core_annual_TL319/sph2m
# ensemble
#file_base=../linkdir/forcing/ensemble_annual_TL319/sph2m
# ncep1
#file_base=../linkdir/forcing/ncep1_annual_TL319/sph2m
# ncep2
#file_base=../linkdir/forcing/ncep2_annual_TL319/sph2m
# ncepc
#file_base=../linkdir/forcing/ncep_cfsr_annual_TL319/sph2m
# merra2
#file_base=../linkdir/forcing/merra2_annual_TL319/sph2m
# 20CRv2
#file_base=../linkdir/forcing/20CRv2_annual_TL319/sph2m
# erai
#file_base=../linkdir/forcing/erai_annual_TL319/sph2m

#fileo=sph2m_glb
#fileo=sph2m_core_glb
#fileo=sph2m_ensemble_glb
#fileo=sph2m_ncep1_glb
#fileo=sph2m_ncep2_glb
#fileo=sph2m_ncepc_glb
#fileo=sph2m_merra2_glb
#fileo=sph2m_20CRv2_glb
#fileo=sph2m_erai_tl319_glb

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
