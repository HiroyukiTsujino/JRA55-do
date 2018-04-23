#! /bin/sh
#
# TS格子点値を全球平均する
#
if [ -z $1 ] ; then
  echo 'Usage: glbmean file_in file_ssh file_out'
  exit 1
fi

fl_in=$1
fl_ssh=$2
fl_out=$3

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./globalmean<<EOF
 &nml_glbmean
 flin="$fl_in"
 flin_ssh="$fl_ssh"
 fltopo="$fltopo"
 flsclf="$flsclf"
 flout="$fl_out"
 /
EOF

