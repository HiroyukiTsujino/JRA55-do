#! /bin/sh
#
# Usage: nwt.sh flin_um flin_vm file_nwt

flin_um=$1
flin_vm=$2
file_nwt=$3

fltopo="../../data/topo.d"
flvgrid="../../data/vgrid.d"
flibas="../../data/basin_map.txt"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./nwt<<EOF
&nml_nwt
 flin_um="$flin_um",
 flin_vm="$flin_vm",
 flout="$file_nwt",
 fltopo="$fltopo",
 flsclf="$flsclf",
 flibas="$flibas",
 /
&inflg
 file_vgrid="$flvgrid"
 /
EOF
