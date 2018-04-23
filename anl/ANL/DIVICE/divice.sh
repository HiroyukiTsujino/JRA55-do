#! /bin/sh
#

flin="$1"
file_out="$2"

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./divice<<EOF
 &nml_divice
 fltopo="$fltopo"
 flsclf="$flsclf"
 flin="$flin"
 flout="$file_out"
 irecu=3
 irecv=4
 /
EOF
