#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/CORE/core_cobesst_annual_dec2015/dlwrf
#fileo=dlwrf_glb_dec2015

#file_base=../linkdir/CORE/core_cobesst_annual_jan2017/dlwrf
#fileo=dlwrf_glb_jan2017

file_base=../linkdir/CORE/core_cobesst_annual_aug2017/dlwrf
fileo=aug2017/dlwrf_glb_aug2017

#file_mask=/worke/htsujino/COBESST/data/topo-jra_cobe_ocean.d
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
