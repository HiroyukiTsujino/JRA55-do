#!/bin/sh
#
# Usage: hs2uvmo flin_u flin_v flin_ssh flout_w, undef

flin_u=${1}
flin_v=${2}
flin_ssh=${3}
flout_ul=${4}
flout_vl=${5}
flout_wl=${6}
l_out_ul=${7}
l_out_vl=${8}
l_out_wl=${9}
undef=${10}

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"
l_upward=.false.

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./gm2wlwl<<EOF
 &nml_gm2wlwl
 flin_u="$flin_u"
 flin_v="$flin_v"
 flin_ssh="$flin_ssh"
 flout_ul="$flout_ul"
 flout_vl="$flout_vl"
 flout_wl="$flout_wl"
 l_out_ul=$l_out_ul
 l_out_vl=$l_out_vl
 l_out_wl=$l_out_wl
 undef=$undef
 fltopo="$fltopo"
 flsclf="$flsclf"
 l_upward=$l_upward
 /
EOF
