#! /bin/sh

x_num=$1
y_num=$2
z_num=$3
var_num=$4
undef=$5
flin1="$6"
factor1=$7
flin2="$8"
factor2=$9
flout="${10}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./lnrcmb<<EOF
 &nml_lnrcmb
  x_num=$x_num
  y_num=$y_num
  z_num=$z_num
  var_num=$var_num
  undef=$undef
  flin1="$flin1"
  factor1=$factor1
  flin2="$flin2"
  factor2=$factor2
  flout="$flout"
 /
EOF

