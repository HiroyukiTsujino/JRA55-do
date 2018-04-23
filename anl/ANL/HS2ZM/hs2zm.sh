#! /bin/sh
#
# echo 'Usage: hs2zm.sh flin flin_ssh flout'

flin="$1"
flin_ssh="$2"
flout="$3"

fltopo="../../data/topo.d"
flvgrid="../../data/vgrid.d"
flibas="../../data/basin_map.txt"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./hs2zm<<EOF
 &nml_zm
 flin="$flin"
 flin_ssh="$flin_ssh"
 flout="$flout"
 fltopo="$fltopo"
 flsclf="$flsclf"
 flibas="$flibas"
 /
 &inflg
 file_vgrid="$flvgrid"
 /
EOF

