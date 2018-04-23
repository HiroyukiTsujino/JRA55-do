#! /bin/sh
#
# TS格子点値を全球平均する
#
if [ -z $1 ] ; then
  echo 'Usage: glbmean2d file_in file_out knum'
  exit 1
fi

fl_in="$1"
fl_out="$2"
knum=$3

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./globalmean2d<<EOF
 &nml_glbm2d
 flin="$fl_in"
 fltopo="$fltopo"
 flsclf="$flsclf"
 flout="$fl_out"
 knum=$knum
 /
EOF

