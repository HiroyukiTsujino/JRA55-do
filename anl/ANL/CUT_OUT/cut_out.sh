#! /bin/sh

x_num=$1
y_num=$2
z_num=$3
var_num=$4
rec_num=$5
flin="$6"
x_stt=${7}
x_end=${8}
y_stt=${9}
y_end=${10}
z_stt=${11}
z_end=${12}
var_stt=${13}
var_end=${14}
rec_stt=${15}
rec_end=${16}
flout="${17}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./cut_out<<EOF
 &nml_cut_out
  x_num=${x_num}
  y_num=${y_num}
  z_num=${z_num}
  var_num=${var_num}
  rec_num=${rec_num}
  flin="$flin"
  x_stt=${x_stt}
  x_end=${x_end}
  y_stt=${y_stt}
  y_end=${y_end}
  z_stt=${z_stt}
  z_end=${z_end}
  var_stt=${var_stt}
  var_end=${var_end}
  rec_stt=${rec_stt}
  rec_end=${rec_end}
  flout="$flout"
 /
EOF

