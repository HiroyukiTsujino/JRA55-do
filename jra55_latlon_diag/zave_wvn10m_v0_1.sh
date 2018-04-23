#!/bin/bash

set -e

ln -sf NAMELIST.MXE.JRA55_ocean_monthly_qscat NAMELIST.MXE

in_dir=/work115/htsujino/SURF_FLUX/forcing/jra55fcst_monthly_TL319
out_dir=/work115/htsujino/SURF_FLUX/forcing/jra55fcst_monthly_TL319

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
