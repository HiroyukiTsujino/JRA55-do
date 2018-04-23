#! /bin/sh

# Usage: day2mon.sh start_year end_year file_core_in file_core_out tuxy k_max UNDEF'

styear=$1
endyear=$2
fpathin="$3"
fpathout="$4"
tuxy="$5"
k_max=$6
undef=$7

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./day2mon<<EOF
 &nml_day2mon
  styear=$styear
  endyear=$endyear
  fpathin="$fpathin"
  fpathout="$fpathout"
  tuxy="$tuxy"
  k_max=$k_max
  UNDEF=$undef
  fltopo="$fltopo"
 /
EOF
