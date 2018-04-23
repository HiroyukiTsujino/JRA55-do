#! /bin/sh
#

flin="$1"
file_out="$2"

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./mag_tau<<EOF
 &nml_mag_tau
 fltopo="$fltopo"
 flin="$flin"
 flout="$file_out"
 /
EOF

