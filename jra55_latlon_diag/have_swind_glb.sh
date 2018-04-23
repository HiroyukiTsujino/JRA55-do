#!/bin/bash

set -e

#ln -sf NAMELIST.MXE.JRA55_ocean_annual NAMELIST.MXE
#ln -sf NAMELIST.MXE.JRA55C_ocean_annual NAMELIST.MXE

# JRA-55 fcst
#file_base=../linkdir/forcing/jra_annual_latlon_e1/swind

# JRA-55 anl
#file_base=../linkdir/forcing/jra_anl_annual_latlon/swind

# JRA-55C 
#file_base=../linkdir/forcing/jra55c_annual_latlon_filt/swind

fileo=swind_glb

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
