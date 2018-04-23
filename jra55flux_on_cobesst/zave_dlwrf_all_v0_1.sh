#!/bin/bash

set -e 

ln -sf NAMELIST.MXE.COBESST.ocean_monthly NAMELIST.MXE

orgdir=../linkdir/forcing/jra55fcst_monthly_1x1
outdir=../linkdir/forcing/jra55fcst_monthly_1x1

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="${orgdir}/dlwrf_all",
  fileo_base="${outdir}/dlwrf_all_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/work116/htsujino/COBESST/data/jra_cobe_ocean-mask.gd"
/
EOF

exit 0
