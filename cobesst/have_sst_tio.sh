#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

file_base=/work116/htsujino/COBESST/annual/grads/sst-glb
fileo=sst_cobe_tio
file_mask=/work116/htsujino/COBESST/data/tio_mask.gd

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
