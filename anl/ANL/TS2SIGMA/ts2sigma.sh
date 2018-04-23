#! /bin/sh
#
#
# Usage: ts2sigma.sh pr_lev flin_t flin_s flout

pr_lev=$1
flin_t="$2"
flin_s="$3"
flout="$4"

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./ts2sigma<<EOF
 &nml_sigma
 plev=$pr_lev
 flin_t="$flin_t"
 flin_s="$flin_s"
 flout="$flout"
 fltopo="$fltopo"
 /
EOF

