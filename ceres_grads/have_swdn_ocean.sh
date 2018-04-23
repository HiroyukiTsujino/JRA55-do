#!/bin/bash

set -e

rm -f NAMELIST.MXE
ln -s NAMELIST.MXE.CERES.annual NAMELIST.MXE 

file_mask=../linkdir/COBESST/data/jra_cobe_ocean-mask.gd
#file_base=../linkdir/CERES/grads_ann/swdn
#fileo=swdn_ceres_ocean
file_base=../linkdir/CERES/grads_ann/swdn_seaice_tropics_filter
fileo=swdn_ceres_seaice_tropics_filter_ocean

l2d=.true.
cgrid=U

exe=have_ctl

./${exe}<<EOF
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=${l2d},
    file_mask="${file_mask}"
    cgrid="${cgrid}",
  /
EOF

rm -f NAMELIST.MXE

exit 0
