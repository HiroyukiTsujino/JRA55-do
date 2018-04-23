#!/bin/bash

orgdir=../linkdir/forcing/jra_cobesst_annual_c2
outdir=../linkdir/forcing/jra_cobesst_annual_c2

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="${orgdir}/tauy",
  fileo_base="${outdir}/tauy_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/work116/htsujino/COBESST/data/jra_cobe_ocean-mask.gd"
/
EOF

exit 0
