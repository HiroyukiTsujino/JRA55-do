#!/bin/bash

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} region_number"
  echo "  region_number = 1,2,3,4,9"
  exit
fi

ln -sf NAMELIST.MXE.COBESST.ocean_monthly NAMELIST.MXE

#orgdir=../linkdir/forcing/jra55fcst_v1_1_monthly_1x1
#outdir=../linkdir/forcing/jra55fcst_v1_1_monthly_1x1

orgdir=../linkdir/forcing/jra55fcst_monthly_1x1
outdir=../linkdir/forcing/jra55fcst_monthly_1x1

exe=zave_ctl

region_number=${1}

file_region[1]=atl
file_region[2]=pac
file_region[3]=ind
file_region[4]=med
file_region[9]=so

echo ${file_region[${region_number}]}

./${exe}<<EOF
&zave_lst
  file_base="${orgdir}/tauy",
  fileo_base="${outdir}/tauy_${file_region[${region_number}]}_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/work116/htsujino/COBESST/data/basin_index.gd"
  i_region_number=${region_number},
/
EOF

exit 0
