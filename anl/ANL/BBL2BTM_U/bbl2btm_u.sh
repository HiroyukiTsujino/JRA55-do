#! /bin/sh
#
# echo 'Usage: bbl2btm_u.sh flinu flout undef_in undef_out'

flinu="$1"
flout="$2"
undef_in=$3
undef_out=$4

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./bbl2btm_u<<EOF
 &nml_bbl2btm_u
 flinu="$flinu"
 flout="$flout"
 fltopo="$fltopo"
 undef_in=${undef_in}
 undef_out=${undef_out}
 /
EOF
