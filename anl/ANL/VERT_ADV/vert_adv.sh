#! /bin/sh
#
# Usage: vert_adv.sh flin_tr flin_wl flin_ssh flout_wt, undef

flin_tr=${1}
flin_wl=${2}
flin_ssh=${3}
flout_wt=${4}
undef=${5}

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./vert_adv<<EOF
&nml_vertadv
  flin_tr="$flin_tr"
  flin_wl="$flin_wl"
  flin_ssh="$flin_ssh"
  flout_wt="$flout_wt"
  undef=$undef
  fltopo="$fltopo"
  flsclf="$flsclf"
 /
EOF
