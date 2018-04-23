#! /bin/sh

x_num=$1
y_num=$2
z_num=$3
var_num=$4
undef=$5
flin="$6"
x_stt=${7}
x_end=${8}
y_stt=${9}
y_end=${10}
z_stt=${11}
z_end=${12}
var_stt=${13}
var_end=${14}
add=${15}
factor=${16}
flout="${17}"


F_RECLUNIT=BYTE    ; export F_RECLUNIT

./replace_range<<EOF
 &nml_replace_range
  x_num=${x_num}
  y_num=${y_num}
  z_num=${z_num}
  var_num=${var_num}
  undef=${undef}
  flin="$flin"
  x_stt=${x_stt}
  x_end=${x_end}
  y_stt=${y_stt}
  y_end=${y_end}
  z_stt=${z_stt}
  z_end=${z_end}
  var_stt=${var_stt}
  var_end=${var_end}
  add=${add}
  factor=${factor}
  flout="$flout"
 /
EOF

