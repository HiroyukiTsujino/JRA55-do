#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_monthly NAMELIST.MXE

orgdir=/worke/htsujino/CORE/core_monthly_cobe_dec2015
outdir=/worke/htsujino/CORE/core_monthly_cobe_dec2015

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="${orgdir}/wind10m",
  fileo_base="${outdir}/wind10m_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/worke/htsujino/COBESST/data/jra_cobe_ocean-mask.gd"
/
EOF

exit 0
