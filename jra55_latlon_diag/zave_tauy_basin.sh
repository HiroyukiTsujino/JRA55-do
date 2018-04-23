#!/bin/bash

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} region_number"
  echo "  region_number = 1,2,3,4,9"
  exit
fi

ln -sf NAMELIST.MXE.JRA55_ocean_monthly_scow NAMELIST.MXE

# v1_4
#orgdir=../linkdir/forcing/jra55fcst_v1_4_prod4_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_v1_4_prod4_monthly_TL319
#orgdir=../linkdir/forcing/jra55fcst_v1_4_prod1_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_v1_4_prod1_monthly_TL319
#
# v1_3
#orgdir=../linkdir/forcing/jra55fcst_v1_3_prod4_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_v1_3_prod4_monthly_TL319
#orgdir=../linkdir/forcing/jra55fcst_v1_3_prod1_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_v1_3_prod1_monthly_TL319
# v1_2
#orgdir=../linkdir/forcing/jra55fcst_v1_2_prod4_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_v1_2_prod4_monthly_TL319
# v0_8
#orgdir=../linkdir/forcing/jra55fcst_v7_prod4_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_v7_prod4_monthly_TL319
# v0_1
#orgdir=../linkdir/forcing/jra55fcst_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_monthly_TL319

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
  file_mask="/work116/htsujino/SURF_FLUX/data/basin_index.gd"
  i_region_number=${region_number},
/
EOF

exit 0
