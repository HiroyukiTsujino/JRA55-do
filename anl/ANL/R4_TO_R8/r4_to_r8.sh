#! /bin/sh

x_num=$1
y_num=$2
z_num=$3
var_num=$4
undefin=$5
flin="$6"
undefout=$7
flout="$8"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./r4_to_r8<<EOF
 &nml_r4_to_r8
  x_num=$x_num
  y_num=$y_num
  z_num=$z_num
  var_num=$var_num
  undefin=$undefin
  flin="$flin"
  undefout=$undefout
  flout="$flout"
 /
EOF

