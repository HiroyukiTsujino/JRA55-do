#!/bin/bash

set -e 

orgdir=/work116/htsujino/CORE/core_cobesst_annual_dec2015
outdir=/work116/htsujino/CORE/core_cobesst_annual_dec2015

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
