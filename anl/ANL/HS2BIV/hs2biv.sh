#! /bin/sh
#
# echo 'Usage: hs2biv.sh flin_t flin_s flin_ssh file_biv k_bound

flin_t=${1}
flin_s=${2}
flin_ssh=${3}
file_biv=${4}
k_bound=${5}

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./hs2biv<<EOF
 &nml_biv
 flin_t="$flin_t"
 flin_s="$flin_s"
 flin_ssh="$flin_ssh"
 flout="$file_biv"
 fltopo="$fltopo"
 flsclf="$flsclf"
 k_bound=$k_bound
 /
EOF

