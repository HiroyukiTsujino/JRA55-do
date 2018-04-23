#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.monthly_clim NAMELIST.MXE

orgdir=/work116/htsujino/CORE/core_cobesst_monthly_dec2015_clim
outdir=/work116/htsujino/CORE/core_cobesst_monthly_dec2015_clim

exe=zave_ctl

for item in dswrf dlwrf
do
./${exe}<<EOF
&zave_lst
  file_base="${orgdir}/${item}_all.jan2001_dec2009",
  fileo_base="${outdir}/${item}_all_zm.jan2001_dec2009"
  l2d=.true.,
  cgrid="U",
  file_mask="/work116/htsujino/COBESST/data/jra_cobe_ocean-mask.gd"
/
EOF

done

exit 0
