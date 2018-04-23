#! /bin/sh
#
# Usage: nst.sh flin_ut flin_vt file_nst

flin_ut=$1
flin_vt=$2
file_nst=$3

fltopo="../../data/topo.d"
flvgrid="../../data/vgrid.d"
flibas="../../data/basin_map.txt"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./nst<<EOF
&nml_nst
 flin_ut="$flin_ut",
 flin_vt="$flin_vt",
 flout="$file_nst",
 fltopo="$fltopo",
 flsclf="$flsclf",
 flibas="$flibas",
 /
&inflg
 file_vgrid="$flvgrid"
 /
EOF
