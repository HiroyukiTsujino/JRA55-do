#! /bin/sh

# Usage: ann2clim_g.sh imut jmut km start_year end_year file_core_in file_core_out

imut=$1
jmut=$2
km=$3
sttyear=$4
endyear=$5
fpathin="$6"
fpathout="$7"
undef=$8
nth_data=$9
l_subclim=${10}
fclim="${11}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./ann2clim_g<<EOF
 &nml_ann2clim_g
  imut=$imut
  jmut=$jmut
  km=$km
  sttyear=$sttyear
  endyear=$endyear
  fpathin="$fpathin"
  fpathout="$fpathout"
  undef=${undef}
  nth_data=${nth_data}
  l_subclim=${l_subclim}
  fclim="${fclim}"
 /
EOF
