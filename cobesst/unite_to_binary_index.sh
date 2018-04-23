#!/bin/bash

set -e

#ln -sf NAMELIST.MXE.COBESST.united_with_jra55 NAMELIST.MXE # first
ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE # ocean only

exe=mk_index_unite
cgrid=U

#indxdir=greenland
#file_mask=greenland_river

#indxdir=region
#file_mask=region_index

#indxdir=basin
#file_mask=basin_index

#indxdir=region_fulldiv
#file_mask=region_index_fulldiv

./${exe}<<EOF
&nml_maskunite
  cgrid="${cgrid}",
  cdir="${indxdir}"
  flout_base="${file_mask}"
/
EOF

rm -f NAMELIST.MXE

exit 0
