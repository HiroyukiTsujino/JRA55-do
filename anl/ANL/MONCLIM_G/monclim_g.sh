#! /bin/sh

# Usage: monclim_g.sh imax jmax kmax start_year end_year file_core_in file_core_out

imax=${1}
jmax=${2}
kmax=${3}
sttyear=${4}
endyear=${5}
undef=${6}
fpathin="${7}"
fpathout="${8}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./monclim_g<<EOF
 &nml_monclim_g
  imax=$imax
  jmax=$jmax
  kmax=$kmax
  sttyear=$sttyear
  endyear=$endyear
  undef=${undef}
  fpathin="$fpathin"
  fpathout="$fpathout"
 /
EOF
