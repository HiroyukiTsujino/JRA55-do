#! /bin/sh
#
# Usage: sigma2mld.sh flin_sigma0 flout_mld

flin_sigma0=${1}
flout_mld=${2}

dsigma0=0.125
undef=0.0

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./sigma2mld<<EOF
 &nml_mld
 flin_sigma0="$flin_sigma0"
 flout_mld="$flout_mld"
 dsigma0=$dsigma0
 undef=$undef
 fltopo="$fltopo"
 /
EOF

