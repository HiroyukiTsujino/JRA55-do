#! /bin/sh
#  wspd.sh  file_uv  file_wind_speed
#

flin="$1"
file_out="$2"

undef=-9.99e33
fltopo="data/topo.d"
file_vgrid="data/vgrid.d"
file_scale="data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./wspd<<EOF
 &nml_wspd
 fltopo="$fltopo"
 file_vgrid="$file_vgrid"
 file_scale="$file_scale"
 flin="$flin"
 undef=${undef}
 flout="$file_out"
 /
EOF

