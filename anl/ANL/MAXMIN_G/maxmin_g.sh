#! /bin/sh

# Usage: x_num y_num z_num var_num undef x0 x1 y0 y1 z0 z1file_core_in file_core_out

x_num=${1}
y_num=${2}
z_num=${3}
var_num=${4}
undef=${5}
x0=${6}
x1=${7}
y0=${8}
y1=${9}
z0=${10}
z1=${11}
fpathin="${12}"
fpathout="${13}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./maxmin_g<<EOF
 &nml_maxmin_g
  x_num=$x_num
  y_num=$y_num
  z_num=$z_num
  var_num=$var_num
  undef=$undef
  x0=$x0
  x1=$x1
  y0=$y0
  y1=$y1
  z0=$z0
  z1=$z1
  fpathin="$fpathin"
  fpathout="$fpathout"
 /
EOF

