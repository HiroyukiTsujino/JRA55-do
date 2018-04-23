#! /bin/sh
#
# Usage: monclim.sh  fincore foutcore tuxy dnum styear endyear

fincore="$1"
foutcore="$2"
tuxy="$3"
dnum=$4
styear=$5
endyear=$6

fltopo="../../data/topo.d"

########################################

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./monclim<<EOF
&nml_monclim
fincore="$fincore"
foutcore="$foutcore"
fltopo="$fltopo"
styear=$styear
endyear=$endyear
tuxy="$tuxy"
dnum=$dnum
/
EOF


