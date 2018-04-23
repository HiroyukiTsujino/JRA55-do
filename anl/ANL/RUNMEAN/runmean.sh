#! /bin/sh

# Usage: x_num y_num z_num var_num undef l_leap y_rage year_stt year_end file_core_in file_core_out

x_num=${1}
y_num=${2}
z_num=${3}
var_num=${4}
undef=${5}
l_leap=${6}
y_range=${7}
year_stt=${8}
year_end=${9}
fpathin="${10}"
fpathout="${11}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./runmean<<EOF
 &nml_runmean
  x_num=$x_num
  y_num=$y_num
  z_num=$z_num
  var_num=$var_num
  undef=$undef
  l_leap=$l_leap
  y_range=$y_range
  year_stt=$year_stt
  year_end=$year_end
  fpathin="$fpathin"
  fpathout="$fpathout"
 /
EOF
