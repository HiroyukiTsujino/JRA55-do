#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_monthly NAMELIST.MXE

orgdir=/work116/htsujino/GPCP-v2_3/grads_monthly_1x1
outdir=/work116/htsujino/GPCP-v2_3/grads_monthly_1x1

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="${orgdir}/precip",
  fileo_base="${outdir}/precip_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/work116/htsujino/COBESST/data/jra_cobe_ocean-mask.gd"
/
EOF

exit 0
