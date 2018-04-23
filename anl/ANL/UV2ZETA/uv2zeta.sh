#! /bin/sh
#  uv2zeta.sh  file_u file_v undef file_zeta
#

fluin="$1"
flvin="$2"
undef=$3
file_out="$4"

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

#namelist /nml_uv2zeta/ fltopo, flsclf, undef, fluin, flvin, flout
./uv2zeta<<EOF
 &nml_uv2zeta
 fltopo="$fltopo"
 flsclf="$flsclf"
 undef=$undef
 fluin="$fluin"
 flvin="$flvin"
 flout="$file_out"
 /
EOF

