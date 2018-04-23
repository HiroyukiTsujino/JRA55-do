#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_monthly NAMELIST.MXE

orgdir=../linkdir/forcing/jra_cobesst_monthly_c0_dec2015_2m
outdir=../linkdir/forcing/jra_cobesst_monthly_c0_dec2015_2m

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
