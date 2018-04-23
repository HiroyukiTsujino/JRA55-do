#! /bin/sh
#
if [ -z $1 ] ; then
  echo 'Usage: zos.sh file_in_ssh file_in_ice file_out file_out_ssh'
  exit 1
fi

flin_ssh=$1
flin_ice=$2
fl_out=$3
flout_ssh=$4

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./zos<<EOF
 &nml_zos
 flin_ssh="$flin_ssh"
 flin_ice="$flin_ice"
 fltopo="$fltopo"
 flsclf="$flsclf"
 flout="$fl_out"
 flout_ssh="$flout_ssh"
 /
EOF
