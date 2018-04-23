#!/bin/bash

set -e

ln -sf NAMELIST.MXE.ERAI.monthly_ocean NAMELIST.MXE

orgdir=/work115/htsujino/ERA-interim/grads_monthly
outdir=/work115/htsujino/ERA-interim/grads_monthly

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="${orgdir}/erai_sph2m",
  fileo_base="${outdir}/sph2m_zm"
  l2d=.true.,
  cgrid="U",
/
EOF

exit 0
