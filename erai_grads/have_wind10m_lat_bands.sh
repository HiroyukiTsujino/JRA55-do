#!/bin/bash

set -e

ln -sf NAMELIST.MXE.ERAI.annual_ocean NAMELIST.MXE

file_base=/work115/htsujino/ERA-interim/grads_annual/erai_wind10m

exe=have_ctl
l2d=.true.
cgrid=U

for bands in 60s40s 40s20s 20s0s 0n20n 20n40n 40n60n
do
fileo=wind10m_erai_${bands}
file_mask=/work115/htsujino/ERA-interim/data/${bands}_mask.gd
./${exe}<<EOF
&have_lst
  file_base="${file_base}",
  fileo="${fileo}",
  l2d=${l2d},
  file_mask="${file_mask}"
  cgrid="${cgrid}",
/
EOF

done

exit 0
