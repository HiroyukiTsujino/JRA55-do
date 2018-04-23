#! /bin/sh
#
# 海洋の子午面循環を計算
#
if [ -z $1 ] ; then
  echo 'Usage: gm2moc.sh fn_out'
  exit 1
fi

fn_out=$1

flin_u="../result/hs_gm_u.$fn_out"
flin_v="../result/hs_gm_v.$fn_out"
flin_ssh="../result/hs_ssh.$fn_out"
file_gmmoc="../result/gmmoc.$fn_out"

fltopo="../../data/topo.d"
flvgrid="../../data/vgrid.d"
flibas="../../data/basin_map.txt"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./gm2moc<<EOF
&nml_gmmoc
 flin_u="$flin_u",
 flin_v="$flin_v",
 flin_ssh="$flin_ssh",
 flout="$file_gmmoc",
 fltopo="$fltopo",
 flsclf="$flsclf",
 flibas="$flibas",
 /
&inflg
 file_vgrid="$flvgrid"
 /
EOF

