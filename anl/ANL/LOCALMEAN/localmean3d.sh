#! /bin/sh
#
#

flin="$1"
flin_ssh="$2"
file_out="$3"

d2_d3=3

kstt=$4
kend=$5
#
lat_b=$6
lat_e=$7
lon_b=$8
lon_e=$9

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"
flvgrid="../../data/vgrid.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./localmean<<EOF
 &nmllocalm
 fltopo="$fltopo"
 flsclf="$flsclf"
 flin="$flin"
 flin_ssh="$flin_ssh"
 d2_d3=$d2_d3
 kstt=$kstt
 kend=$kend
 flout="$file_out"
 lat_b=$lat_b
 lat_e=$lat_e
 lon_b=$lon_b
 lon_e=$lon_e
 /
 &inflg
 file_vgrid="$flvgrid"
 /
EOF

