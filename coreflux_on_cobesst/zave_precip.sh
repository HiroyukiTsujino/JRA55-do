#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_monthly NAMELIST.MXE

orgdir=/work116/htsujino/CORE/core_cobesst_monthly_aug2017
outdir=/work116/htsujino/CORE/core_cobesst_monthly_aug2017
#orgdir=/work116/htsujino/CORE/core_cobesst_monthly_jan2017
#outdir=/work116/htsujino/CORE/core_cobesst_monthly_jan2017
#orgdir=/work116/htsujino/CORE/core_cobesst_monthly_dec2015
#outdir=/work116/htsujino/CORE/core_cobesst_monthly_dec2015

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
