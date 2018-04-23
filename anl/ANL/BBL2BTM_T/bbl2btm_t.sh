#! /bin/sh
#
# echo 'Usage: bbl2btm_t.sh flin flout undef_in undef_out'

flin="$1"
flout="$2"
undef_in=$3
undef_out=$4

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./bbl2btm_t<<EOF
 &nml_bbl2btm_t
 flin="$flin"
 flout="$flout"
 fltopo="$fltopo"
 undef_in=${undef_in}
 undef_out=${undef_out}
 /
EOF
