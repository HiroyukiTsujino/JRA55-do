#! /bin/sh
# Usage: mon2ann_g.sh imut jmut km start_year end_year file_core_in file_core_out undef

set -e

imut=$1
jmut=$2
km=$3
styear=$4
endyear=$5
fpathin="$6"
fpathout="$7"
undef=$8

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./mon2ann_g<<EOF
 &nml_mon2ann_g
  imut=$imut
  jmut=$jmut
  km=$km
  l_leap_valid=.true.
  sttyear=$styear
  endyear=$endyear
  fpathin="$fpathin"
  fpathout="$fpathout"
  undef=$undef
 /
EOF
