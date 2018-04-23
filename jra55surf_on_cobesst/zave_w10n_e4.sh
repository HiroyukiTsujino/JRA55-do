#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_monthly NAMELIST.MXE

orgdir=/work115/htsujino/SURF_FLUX/forcing/jra_cobesst_monthly_e4
outdir=/work115/htsujino/SURF_FLUX/forcing/jra_cobesst_monthly_e4

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="${orgdir}/wvn10m",
  fileo_base="${outdir}/wvn10m_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/work116/htsujino/COBESST/data/jra_cobe_ocean-mask.gd"
/
EOF

exit 0
