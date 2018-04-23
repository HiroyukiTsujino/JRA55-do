#! /bin/sh

flin="$1"
flout="$2"
addval=$3
factor=$4
undef=$5
tuxy="$6"
k_max=$7

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./replace<<EOF
 &nml_replace
  fltopo="$fltopo"
  flin="$flin"
  flout="$flout"
  addval=$addval
  factor=$factor
  undef=$undef
  tuxy="$tuxy"
  k_max=$k_max
 /
EOF

