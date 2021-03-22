#!/bin/bash

set -e

#ln -sf NAMELIST.MXE.COBESST.united_with_jra55 NAMELIST.MXE # first
ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE # ocean only

exe=mk_mask_unite
cgrid=U

#indxdir=60s40s
#file_mask=60s40s_mask

#indxdir=40s20s
#file_mask=40s20s_mask

#indxdir=20s0s
#file_mask=20s0s_mask

#indxdir=0n20n
#file_mask=0n20n_mask

#indxdir=20n40n
#file_mask=20n40n_mask

#indxdir=40n60n
#file_mask=40n60n_mask

#indxdir=60s60n
#file_mask=60s60n_mask

#indxdir=tropics
#file_mask=tropics_mask

#indxdir=so
#file_mask=so_mask

#indxdir=nh
#file_mask=nh_mask

#indxdir=np
#file_mask=np_mask

#indxdir=wtp
#file_mask=wtp_mask

#indxdir=wtp2
#file_mask=wtp2_mask

#indxdir=tio
#file_mask=tio_mask

#indxdir=pac
#file_mask=pac_mask

#indxdir=pac_sv
#file_mask=pac_sv_mask

#indxdir=med_black
#file_mask=med_black_mask

#indxdir=mediterranean
#file_mask=med_mask

#indxdir=greenland
#file_mask=greenland_river

#indxdir=ocean
#file_mask=ocean_mask

#indxdir=sp60s
#file_mask=sp60s

#indxdir=60nnp
#file_mask=60nnp

#indxdir=enso
#file_mask=enso

#indxdir=pdo
#file_mask=pdo

./${exe}<<EOF
&nml_maskunite
  cgrid="${cgrid}",
  cdir="${indxdir}"
  flout_base="${file_mask}"
/
EOF

#mv ocean_mask.gd /work116/htsujino/COBESST/data/jra_cobe_ocean-mask.gd
#mv ocean_mask.d  /work116/htsujino/COBESST/data/topo-jra_cobe_ocean.d

rm -f NAMELIST.MXE

exit 0
