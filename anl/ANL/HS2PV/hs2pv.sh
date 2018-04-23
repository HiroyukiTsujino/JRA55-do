#!/bin/bash
#
if [ -z $1 ] ; then
  echo 'Usage: hs2pv.sh extension'
  exit 1
fi

cdate=${1}

echo ${cdate}

flin_u="../grddat/hs_u.$cdate"
flin_v="../grddat/hs_v.$cdate"
flin_t="../grddat/hs_t.$cdate"
flin_s="../grddat/hs_s.$cdate"
flinssh="../grddat/hs_ssh.$cdate"
file_pv="../grddat/hs_pv.$cdate"

refpress=0.d0
#refpress=200.d0
#refpress=400.d0

fltopo="../../data/topo.d"
flvgrid="../../data/vgrid.d"
flibas="../../data/basin_map.txt"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./hs2pv<<EOF
 &nml_pv
 refpress=$refpress
 flin_u="$flin_u"
 flin_v="$flin_v"
 flin_t="$flin_t"
 flin_s="$flin_s"
 flinssh="$flinssh"
 flout="$file_pv"
 fltopo="$fltopo"
 flsclf="$flsclf"
 /
 &inflg
 file_vgrid="$flvgrid"
 /
EOF
