#!/bin/bash

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/forcing/jra_cobesst_annual_org/latent
#fileo=latent_glb_org
#file_base=../linkdir/forcing/jra_cobesst_annual_c0/latent
#fileo=latent_glb_c0
#file_base=../linkdir/forcing/jra_cobesst_annual_c2/latent
#fileo=latent_glb_c2_ly2009
#file_base=../linkdir/forcing/jra_cobesst_annual_d2/latent
#fileo=latent_glb_d2
#file_base=../linkdir/forcing/jra_cobesst_annual_d5/latent
#fileo=latent_glb_d5
#file_base=../linkdir/forcing/jra_cobesst_annual_d6/latent
#fileo=latent_glb_d6
#file_base=../linkdir/forcing/jra_cobesst_annual_e3/latent
#fileo=latent_glb_e3
file_base=../linkdir/forcing/jra_cobesst_annual_e4/latent
fileo=latent_20S20N_e4
#file_base=../linkdir/forcing/jra_cobesst_annual_e4_ly2009/latent
#fileo=latent_glb_e4_ly2009
#file_base=../linkdir/forcing/jra_cobesst_annual_e5/latent
#fileo=latent_glb_e5

file_mask=/work116/htsujino/COBESST/data/tropics_mask.gd
l2d=.true.
cgrid=U

exe=have_ctl

./${exe}<<EOF
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=${l2d},
    cgrid="${cgrid}",
    file_mask="${file_mask}"
  /
EOF

exit 0
