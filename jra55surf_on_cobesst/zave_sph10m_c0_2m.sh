#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_monthly NAMELIST.MXE

orgdir=/work116/htsujino/SURF_FLUX/forcing/jra_cobesst_monthly_c0_dec2015_2m
outdir=/work116/htsujino/SURF_FLUX/forcing/jra_cobesst_monthly_c0_dec2015_2m

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="${orgdir}/sph10m",
  fileo_base="${outdir}/sph10m_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/work116/htsujino/COBESST/data/jra_cobe_ocean-mask.gd"
/
EOF

exit 0
