#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#basedir=../linkdir/forcing
basedir=/work115/htsujino/SURF_FLUX/forcing

#file_base=${basedir}/jra_cobesst_annual_c2/dqu10m
#fileo=dqu10m_glb_c2

file_base=${basedir}/jra_cobesst_annual_e4/dqu10m
fileo=dqu10m_glb_e4

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
