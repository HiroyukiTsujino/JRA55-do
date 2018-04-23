#! /bin/sh
#
#  xfactor x_num y_num undef flin1 z_num_1 var_num_1 flin2 z_num_2 var_num_2 flout
#

x_num=$1
y_num=$2
undef=$3
flin1="$4"
z_num_1=$5
var_num_1=$6
flin2="$7"
z_num_2=$8
var_num_2=$9
flout="${10}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./xfactor<<EOF
 &nml_xfactor
  x_num=$x_num
  y_num=$y_num
  undef=$undef
  flin1="$flin1"
  z_num_1=$z_num_1
  var_num_1=$var_num_1
  flin2="$flin2"
  z_num_2=$z_num_2
  var_num_2=$var_num_2
  flout="$flout"
 /
EOF

