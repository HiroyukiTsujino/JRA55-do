#!/bin/bash

set -e

ln -sf NAMELIST.MXE.JRA55ocean_monthly NAMELIST.MXE

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="/work115/htsujino/SURF_FLUX/forcing/jra_monthly_e3/wvn10m",
  fileo_base="/work115/htsujino/SURF_FLUX/forcing/jra_monthly_e3/wvn10m_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="../linkdir/data/jra55_ocean_mask.gd"
/
EOF

exit 0
