#! /bin/sh
#
if [ -z $1 ] ; then
  echo 'Usage: gm2mocip.sh  yyyymm'
  exit 1
fi

yyyymm=$1

flin_u="../result/hs_gm_u.$yyyymm"
flin_v="../result/hs_gm_v.$yyyymm"
flin_t="../result/hs_t.$yyyymm"
flin_s="../result/hs_s.$yyyymm"
flinssh="../result/hs_ssh.$yyyymm"
file_mocip="../result/gmmocip4.$yyyymm"

# -- sigma_2 --
#refpress=200.d0
#rho_max=37.5d0

# -- sigma_4 --
refpress=400.d0
rho_max=46.2d0

fltopo="../../data/topo.d"
flvgrid="../../data/vgrid.d"
flibas="../../data/basin_map.txt"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./gm2mocip<<EOF
 &nml_gmmocip
 refpress=$refpress
 rho_max=$rho_max
 flin_u="$flin_u"
 flin_v="$flin_v"
 flin_t="$flin_t"
 flin_s="$flin_s"
 flinssh="$flinssh"
 flout="$file_mocip"
 fltopo="$fltopo"
 flsclf="$flsclf"
 flibas="$flibas"
 /
 &inflg
 file_vgrid="$flvgrid"
 /
EOF

