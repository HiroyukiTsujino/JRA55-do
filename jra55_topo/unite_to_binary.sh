#!/bin/bash

set -e

exe=mk_mask_unite
cgrid=U

#ln -sf NAMELIST.MXE.JRA55ocean NAMELIST.MXE
#ln -sf NAMELIST.MXE.JRA55 NAMELIST.MXE

#indxdir=60s60n
#file_mask=60s60n_mask

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

#indxdir=tropics
#file_mask=tropics_mask

#indxdir=tropical_buoys
#file_mask=tropical_buoys_mask

#indxdir=so
#file_mask=so_mask

#indxdir=np
#file_mask=np_mask

#indxdir=wtp
#file_mask=wtp_mask

#indxdir=tio
#file_mask=tio_mask

#indxdir=pac
#file_mask=pac_mask

#indxdir=satell_wind
#file_mask=satell_wind_mask

#indxdir=okh_jpn
#file_mask=okh_jpn

#indxdir=okh_jpn_med
#file_mask=okh_jpn_med

#indxdir=jpn_model
#file_mask=jpn_model

#indxdir=ensemble3
#file_mask=ensemble3

#indxdir=ensemble4
#file_mask=ensemble4

#indxdir=mediterranean
#file_mask=med_mask

#ln -sf NAMELIST.MXE.JRA55 NAMELIST.MXE
#indxdir=ocean
#file_mask=jra55_ocean

./${exe}<<EOF
&nml_maskunite
  cgrid="${cgrid}",
  cdir="${indxdir}"
  flout_base="${file_mask}"
/
EOF

rm -f NAMELIST.MXE

exit 0
