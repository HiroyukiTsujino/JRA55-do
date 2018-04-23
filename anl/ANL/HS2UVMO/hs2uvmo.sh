#! /bin/sh
#
# Usage: hs2uvmo flin_u flin_v flin_ssh file_umo file_vmo

flin_u=$1
flin_v=$2
flin_ssh=$3
file_umo=$4
file_vmo=$5

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./hs2uvmo<<EOF
 &nml_uvmo
 flin_u="$flin_u"
 flin_v="$flin_v"
 flin_ssh="$flin_ssh"
 fltopo="$fltopo"
 flsclf="$flsclf"
 flout_umo="$file_umo"
 flout_vmo="$file_vmo"
 /
EOF

