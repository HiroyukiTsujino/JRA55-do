#!/bin/bash
#
if [ -z $1 ] ; then
  echo 'Usage: hs2mocip.sh year date'
  exit 1
fi

#year=${1}
cdate=${1}

echo ${cdate}

flin_u="../grdmon/hs_u.$cdate"
flin_v="../grdmon/hs_v.$cdate"
flin_t="../grdmon/hs_t.$cdate"
flin_s="../grdmon/hs_s.$cdate"
flinssh="../grdmon/hs_ssh.$cdate"
file_mocip="../grdmon/mocip2_half.$cdate"

# -- sigma_2 --
refpress=200.d0
rho_sep=37.5d0,36.4d0,34.0d0,29.d0,
rho_int=0.02d0,0.1d0,0.25d0,

# -- sigma_4 --
#refpress=400.d0
#rho_max=46.2d0

fltopo="../../data/topo.d"
flvgrid="../../data/vgrid.d"
flibas="../../data/basin_map.txt"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./hs2mocip<<EOF
 &nml_mocip
 refpress=$refpress
 rho_sep=$rho_sep
 rho_int=$rho_int
 flin_u="$flin_u"
 flin_v="$flin_v"
 flin_t="$flin_t"
 flin_s="$flin_s"
 flinssh="$flinssh"
 flout="$file_mocip"
 fltopo="$fltopo"
 flsclf="$flsclf"
 flibas="$flibas"
 /
 &inflg
 file_vgrid="$flvgrid"
 /
EOF
