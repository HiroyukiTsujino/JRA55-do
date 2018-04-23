#! /bin/sh
#
#
# Usage: ts2density.sh l_insitu pr_lev flin_t flin_s flout

insitu=${1}
pr_lev=${2}
flin_t="${3}"
flin_s="${4}"
flout="${5}"

fltopo="../../data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./ts2density<<EOF
 &nml_density
 l_insitu=${insitu}
 plev=${pr_lev}
 flin_t="${flin_t}"
 flin_s="${flin_s}"
 flout="${flout}"
 fltopo="${fltopo}"
 /
EOF
