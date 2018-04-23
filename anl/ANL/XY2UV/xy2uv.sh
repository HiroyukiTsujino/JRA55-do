#! /bin/sh
#
yyyymm=$1

flinx="../result/hs_gm_u.$yyyymm"
fliny="../result/hs_gm_v.$yyyymm"
flout="../result/gm_uv.$yyyymm"

fltopo="../../data/topo.d"

./xy2uv<<EOF
&nml_fpath
 flinx="$flinx",
 fliny="$fliny",
 flout="$flout",
 fltopo="$fltopo",
/
EOF
