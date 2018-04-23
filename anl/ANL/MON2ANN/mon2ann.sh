#! /bin/sh

if [ -z $6 ] ; then
  echo 'Usage: mon2ann.sh start_year end_year file_core_in file_core_out tuxy k_max'
fi

styear=$1
endyear=$2
fpathin="$3"
fpathout="$4"
tuxy="$5"
k_max=$6

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./mon2ann<<EOF
 &nml_mon2ann
  styear=$styear
  endyear=$endyear
  fpathin="$fpathin"
  fpathout="$fpathout"
  tuxy="$tuxy"
  k_max=$k_max
  fltopo="$fltopo"
 /
EOF
