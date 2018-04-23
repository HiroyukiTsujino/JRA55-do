#! /bin/sh
#
# Usage: passage.sh  flin_u flin_v flin_ssh file_pass      \
#                    lon_left lat_left lon_right lat_right \
#                    l_cmip5
#

flin_u=$1
flin_v=$2
flin_ssh=$3
file_pass=$4
lon_left=$5
lat_left=$6
lon_right=$7
lat_right=$8
l_cmip5=$9

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"
flvgrid="../../data/vgrid.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./passage<<EOF
&nml_pass
 flin_u="$flin_u",
 flin_v="$flin_v",
 flin_ssh="$flin_ssh",
 lon_left=$lon_left
 lat_left=$lat_left
 lon_right=$lon_right
 lat_right=$lat_right
 flout="$file_pass",
 fltopo="$fltopo",
 flsclf="$flsclf",
 l_cmip5=$l_cmip5
/
&inflg
 file_vgrid="$flvgrid"
 /
EOF

