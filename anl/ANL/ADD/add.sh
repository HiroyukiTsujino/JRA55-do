#! /bin/sh

flin1="$1"
flin2="$2"
flout="$3"
undef=$4
tuxy="$5"
k_max=$6

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./add<<EOF
 &nml_add
  fltopo="$fltopo"
  flin1="$flin1"
  flin2="$flin2"
  flout="$flout"
  undef=$undef
  tuxy="$tuxy"
  k_max=$k_max
 /
EOF

