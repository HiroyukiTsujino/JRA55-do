#! /bin/sh

# Usage: day2mon.sh year month file_core_in file_core_out tuxy k_max UNDEF'

iyear=$1
imonth=$2
fpathin="$3"
fpathout="$4"
tuxy="$5"
k_max=$6
undef=$7

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./day2mon_ym<<EOF
 &nml_day2mon_ym
  iyear=$iyear
  imonth=$imonth
  fpathin="$fpathin"
  fpathout="$fpathout"
  tuxy="$tuxy"
  k_max=$k_max
  UNDEF=$undef
  fltopo="$fltopo"
 /
EOF
