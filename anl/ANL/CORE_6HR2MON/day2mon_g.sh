#! /bin/sh

# Usage: x_num y_num z_num var_num undef year l_leap file_core_in file_core_out

x_num=${1}
y_num=${2}
z_num=${3}
var_num=${4}
undef=${5}
iyear=${6}
l_leap=${7}
fpathin="${8}"
fpathout="${9}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./day2mon_g<<EOF
 &nml_day2mon_g
  x_num=$x_num
  y_num=$y_num
  z_num=$z_num
  var_num=$var_num
  undef=$undef
  iyear=$iyear
  l_leap=$l_leap
  fpathin="$fpathin"
  fpathout="$fpathout"
 /
EOF
