#!/bin/bash

set -e

ln -sf NAMELIST.MXE.CERES.monclim NAMELIST.MXE

#period=jan2001_dec2009

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="../linkdir/CERES/grads_clim/lwdn",
  fileo_base="../linkdir/CERES/grads_clim/lwdn_zm.${period}"
  l2d=.true.,
  cgrid="U",
  file_mask="../linkdir/COBESST/data/jra_cobe_ocean-mask.gd"
/
EOF

exit 0
