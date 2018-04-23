#! /bin/sh
#
# Usage: nht.sh flin_ut flin_vt file_nht

flin_ut=$1
flin_vt=$2
file_nht=$3

fltopo="../../data/topo.d"
flvgrid="../../data/vgrid.d"
flibas="../../data/basin_map.txt"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./nht<<EOF
&nml_nht
 flin_ut="$flin_ut",
 flin_vt="$flin_vt",
 flout="$file_nht",
 fltopo="$fltopo",
 flsclf="$flsclf",
 flibas="$flibas",
 /
&inflg
 file_vgrid="$flvgrid"
 /
EOF

