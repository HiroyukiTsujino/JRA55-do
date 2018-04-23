#! /bin/sh
#
# Usage: hs2uvmo flin_u flin_v flin_ssh flout_w, undef

flin_u=$1
flin_v=$2
flin_ssh=$3
flout_w=$4
undef=$5

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./uv2w<<EOF
 &nml_uv2w
 flin_u="$flin_u"
 flin_v="$flin_v"
 flin_ssh="$flin_ssh"
 flout_w="$flout_w"
 undef=$undef
 fltopo="$fltopo"
 flsclf="$flsclf"
 /
EOF

