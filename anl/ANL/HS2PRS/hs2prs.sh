#! /bin/sh
#
# echo 'Usage: hs2prs.sh flin_t flin_s flin_ssh file_prs

flin_t=${1}
flin_s=${2}
flin_ssh=${3}
file_prs=${4}

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./hs2prs<<EOF
 &nml_pressure
 flin_t="$flin_t"
 flin_s="$flin_s"
 flin_ssh="$flin_ssh"
 flout="$file_prs"
 fltopo="$fltopo"
 flsclf="$flsclf"
 /
EOF

