#! /bin/sh
#
#

flin="$1"
file_out="$2"

d2_d3=2

kstt=1
kend=${3}

lat_b=$4
lat_e=$5
lon_b=$6
lon_e=$7
tuw="${8}"
undef=${9}
lmskout=${10}

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"
flvgrid="../../data/vgrid.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./localmean_mod<<EOF
 &nmllocalm
 fltopo="$fltopo"
 flsclf="$flsclf"
 flin="$flin"
 d2_d3=$d2_d3
 kstt=$kstt
 kend=$kend
 flout="$file_out"
 lat_b=$lat_b
 lat_e=$lat_e
 lon_b=$lon_b
 lon_e=$lon_e
 tuw="${tuw}"
 undef4=$undef
 lmskout=$lmskout
 /
 &inflg
 file_vgrid="$flvgrid"
 /
EOF
