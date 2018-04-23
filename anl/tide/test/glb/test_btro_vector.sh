#!/bin/bash


ln -sf test/glb/namelist.configure.in .
ln -sf test/glb/namelist.test .


driver=btro_vector_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &nml_btro_vector
    file_base_um="../../linkdir/mxe/glb/snp/hs_sfc_snp_um",
    file_base_vm="../../linkdir/mxe/glb/snp/hs_sfc_snp_vm",
  /
EOF

exit 0
