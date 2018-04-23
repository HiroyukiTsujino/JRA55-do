#! /bin/sh

x_num=$1
y_num=$2
z_num=$3
var_num=$4
undef=$5
flin1="$6"
flin2="$7"
flout="$8"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./overwrite<<EOF
 &nml_overwrite
  x_num=${x_num}
  y_num=${y_num}
  z_num=${z_num}
  var_num=${var_num}
  undef=${undef}
  flin1="$flin1"
  flin2="$flin2"
  flout="$flout"
 /
EOF

