#! /bin/sh
#

flin="$1"
file_out="$2"

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./divgtau<<EOF
 &nml_divgtau
 fltopo="$fltopo"
 flsclf="$flsclf"
 flin="$flin"
 flout="$file_out"
 irecu=1
 irecv=2
 /
EOF
