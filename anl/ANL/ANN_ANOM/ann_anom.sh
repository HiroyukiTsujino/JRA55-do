#! /bin/sh

# Usage: x_num y_num z_num var_num undef xwrite, ywrite, zwrite, vwrite, \
# year_start year_end file_core_in0 file_core_in file_core_out

x_num=${1}
y_num=${2}
z_num=${3}
var_num=${4}
undef=${5}
yearstt=${6}
yearend=${7}
fpathin0="${8}"
fpathin="${9}"
fpathout="${10}"
l_readclim=${11}
flclm="${12}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./ann_anom<<EOF
 &nml_ann_anom
  x_num=$x_num
  y_num=$y_num
  z_num=$z_num
  var_num=$var_num
  undef=$undef
  yearstt=$yearstt
  yearend=$yearend
  fpathin="$fpathin"
  fpathin0="$fpathin0"
  fpathout="$fpathout"
  l_readclim=${l_readclim}
  flclm="$flclm"
 /
EOF
