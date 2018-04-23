#! /bin/sh

# Usage: x_num y_num z_num var_num undef xwrite, ywrite, zwrite, vwrite, \
# year_start year_end file_core_in0 file_core_in file_core_out

x_num=${1}
y_num=${2}
z_num=${3}
var_num=${4}
undef=${5}
xwrite=${6}
ywrite=${7}
zwrite=${8}
vwrite=${9}
yearstt=${10}
yearend=${11}
fpathin0="${12}"
fpathin="${13}"
fpathout="${14}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./mon_anom<<EOF
 &nml_mon_anom
  x_num=$x_num
  y_num=$y_num
  z_num=$z_num
  var_num=$var_num
  undef=$undef
  xwrite=$xwrite
  ywrite=$ywrite
  zwrite=$zwrite
  vwrite=$vwrite
  yearstt=$yearstt
  yearend=$yearend
  fpathin0="$fpathin0"
  fpathin="$fpathin"
  fpathout="$fpathout"
 /
EOF
