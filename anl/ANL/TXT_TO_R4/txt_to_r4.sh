#! /bin/sh

flin="${1}"
t_num=${2}
flout="${3}"

x_num=1
y_num=1
z_num=1
var_num=1

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./txt_to_r4<<EOF
 &nml_txt_to_r4
  x_num=$x_num
  y_num=$y_num
  z_num=$z_num
  var_num=$var_num
  t_num=$t_num
  flin="${flin}"
  flout="${flout}"
 /
EOF

