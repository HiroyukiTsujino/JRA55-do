#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/CORE/core_cobesst_annual_dec2015/dswrf
#fileo=dswrf_glb_dec2015

#file_base=../linkdir/CORE/core_cobesst_annual_jan2017/dswrf
#fileo=dswrf_glb_jan2017

file_base=../linkdir/CORE/core_cobesst_annual_aug2017/dswrf
fileo=aug2017/dswrf_glb_aug2017

#file_mask=/work116/htsujino/COBESST/data/jra_cobe_ocean-mask.gd
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

rm -f NAMELIST.MXE

exit 0
