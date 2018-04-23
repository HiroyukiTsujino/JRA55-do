#! /bin/sh
#
# echo 'Usage: hs2btm_t.sh flin flout'

flin="$1"
flout="$2"

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./hs2btm_t<<EOF
 &nml_hs2btm_t
 flin="$flin"
 flout="$flout"
 fltopo="$fltopo"
 /
EOF

