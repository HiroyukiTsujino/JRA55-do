#! /bin/sh
#

flin="$1"
file_out="$2"

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./curltau<<EOF
 &nml_curltau
 fltopo="$fltopo"
 flsclf="$flsclf"
 flin="$flin"
 flout="$file_out"
 /
EOF

