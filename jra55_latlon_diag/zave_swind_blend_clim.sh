#!/bin/bash

set -e

ln -sf NAMELIST.MXE.JRA55.clim NAMELIST.MXE

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="/work115/htsujino/SURF_FLUX/forcing/jra_blend_clim_latlon/swind_clim",
  fileo_base="/work115/htsujino/SURF_FLUX/forcing/jra_blend_clim_latlon/swind_clim_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="../linkdir/data/jra55_ocean_mask.gd"
/
EOF

exit 0
