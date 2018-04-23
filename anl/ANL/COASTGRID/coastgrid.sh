#! /bin/sh
#

fout_core=${1}

fltopo="topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./coastgrid<<EOF
 &nml_coastgrid
  fout_core="${fout_core}"
  fltopo="${fltopo}"
 /
EOF

