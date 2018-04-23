#!/bin/bash

set -e

orgdir=../linkdir/forcing/jra55fcst_v7_rad2_annual_1x1
outdir=../linkdir/forcing/jra55fcst_v7_rad2_annual_1x1

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="${orgdir}/taux",
  fileo_base="${outdir}/taux_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/work116/htsujino/COBESST/data/jra_cobe_ocean-mask.gd"
/
EOF

exit 0
