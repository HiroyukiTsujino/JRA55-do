#! /bin/sh
#
# echo 'Usage: hs2btm_t.sh flinu flinv flout'

flinu="$1"
flinv="$2"
flout="$3"

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./hs2btm_uv<<EOF
 &nml_hs2btm_uv
 flinu="$flinu"
 flinv="$flinv"
 flout="$flout"
 fltopo="$fltopo"
 /
EOF

