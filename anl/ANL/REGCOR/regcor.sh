#! /bin/sh
#
# Usage: regcor.sh fseriescore snum fincore foutcore tuxy dnum styear endyear lag

fseriescore="$1"
snum=$2
fincore="$3"
foutcore="$4"
tuxy="$5"
dnum=$6
styear=$7
endyear=$8
lag=$9

fltopo="../../data/topo.d"

########################################

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./regcor<<EOF
&nml_regcor
fseriescore="$fseriescore"
snum=$snum
fincore="$fincore"
foutcore="$foutcore"
tuxy="$tuxy"
dnum=$dnum
fltopo="$fltopo"
styear=$styear
endyear=$endyear
lag=$lag
/
EOF


