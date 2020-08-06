#!/bin/bash

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} region_number"
  echo "  region_number = 1,2,3,4,9"
  exit
fi

ln -sf NAMELIST.MXE.SCOW.monthly NAMELIST.MXE

orgdir=/work116/htsujino/SCOW/grads
outdir=/work116/htsujino/SCOW/grads

exe=zave_ctl

region_number=${1}

./${exe}<<EOF
&zave_lst
  file_base="${orgdir}/tauy",
  fileo_base="${outdir}/tauy_glb_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/work116/htsujino/WOA13v2/data/basin_index.gd"
  i_region_number=-9,
  ex_region_number=8,
/
EOF

exit 0
