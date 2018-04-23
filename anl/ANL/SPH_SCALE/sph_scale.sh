#! /bin/sh

# Usage: sph_scale.sh scale_factor.d  [file_vgrid]

flsclf=${1}

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./sph_scale<<EOF
 &nml_sph_scale
  flsclf="$flsclf"
 /
EOF
