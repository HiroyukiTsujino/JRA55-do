#! /bin/sh

x_num=$1
y_num=$2
z_num=$3
var_num=$4
flin="$5"
flout="$6"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./mask_uv2ts<<EOF
 &nml_mask_uv2ts
  x_num=$x_num
  y_num=$y_num
  z_num=$z_num
  var_num=$var_num
  flin="$flin"
  flout="$flout"
 /
EOF
