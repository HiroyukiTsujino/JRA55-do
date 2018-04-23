#! /bin/sh
#
#

flin="$1"
flout="$2"

undef=$3
tuxy="$4"

k_2d=$5

i_begin=$6
i_end=$7

j_begin=$8
j_end=$9

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./glb2rgn_ij<<EOF
 &nml_glb2rgn_ij
 flin="$flin"
 flout="$flout"
 fltopo="$fltopo"
 undef=$undef
 tuxy="$tuxy"
 k_2d=$k_2d
 i_begin=$i_begin
 i_end=$i_end
 j_begin=$j_begin
 j_end=$j_end
 /
EOF

