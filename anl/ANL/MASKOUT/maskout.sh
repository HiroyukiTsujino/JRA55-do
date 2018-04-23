#! /bin/sh

x_num=$1
y_num=$2
z_num=$3
var_num=$4
lwlim=$5
uplim=$6
flin="$7"
undef=$8
flout="$9"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./maskout<<EOF
 &nml_maskout
  x_num=$x_num
  y_num=$y_num
  z_num=$z_num
  var_num=$var_num
  lwlim=$lwlim
  uplim=$uplim
  flin="$flin"
  undef=$undef
  flout="$flout"
 /
EOF

