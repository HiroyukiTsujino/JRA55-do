#! /bin/sh
#
#

flin="$1"
flin_ssh="$2"
file_out="$3"
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

./ht_content<<EOF
 &nml_local_fw
 fltopo="$fltopo"
 flsclf="$flsclf"
 flin="$flin"
 flin_ssh="$flin_ssh"
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
