#! /bin/sh
#
# 海洋の子午面循環を計算
#

lon_left=$1
lat_left=$2
lon_right=$3
lat_right=$4
namepass=$5
yyyymm=$6

flin_ice_uv="../result/hs_icecat_uv.$yyyymm"
file_pass="../logs/PASS/$namepass.$yyyymm"
fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"
flvgrid="../../data/vgrid.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./passage_ice<<EOF
&nml_passi
 flin_ice_uv="$flin_ice_uv",
 irecu=3,
 irecv=4,
 lon_left=$lon_left
 lat_left=$lat_left
 lon_right=$lon_right
 lat_right=$lat_right
 flout="$file_pass",
 fltopo="$fltopo",
 flsclf="$flsclf",
/
&inflg
 file_vgrid="$flvgrid"
 /
EOF
