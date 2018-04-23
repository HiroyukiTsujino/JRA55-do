#! /bin/sh
#
# barotropic streamfunction from x-ward transport [Sv]
#

flin_u=$1
flin_ssh=$2
file_out=$3

fltopo="../../data/topo.d"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./btsf<<EOF
 &nml_btsf
 flin_u="$flin_u"
 flin_ssh="$flin_ssh"
 fltopo="$fltopo"
 flsclf="$flsclf"
 flout="$file_out"
 /
EOF

