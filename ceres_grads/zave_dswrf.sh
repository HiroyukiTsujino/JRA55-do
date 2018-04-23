#!/bin/bash

set -e

ln -sf NAMELIST.MXE.CERES.monthly NAMELIST.MXE

exe=zave_ctl

#basename=swdn
basename=swdn_seaice_tropics_filter

./${exe}<<EOF
&zave_lst
  file_base="../linkdir/CERES/grads/${basename}",
  fileo_base="../linkdir/CERES/grads/${basename}_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/work116/htsujino/COBESST/data/jra_cobe_ocean-mask.gd"
/
EOF

exit 0
