#!/bin/bash

set -e

ln -sf NAMELIST.MXE.JRA55_ocean_monthly_qscat NAMELIST.MXE

#in_dir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_3_prod4_monthly_TL319
#out_dir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_3_prod4_monthly_TL319

#in_dir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_4_prod1_monthly_TL319
#out_dir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_4_prod1_monthly_TL319

#in_dir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_4_prod4_monthly_TL319
#out_dir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_4_prod4_monthly_TL319

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="${in_dir}/wvn10m",
  fileo_base="${out_dir}/wvn10m_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="../linkdir/data/jra55_ocean_mask.gd"
/
EOF

exit 0
