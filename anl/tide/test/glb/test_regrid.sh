#!/bin/bash


ln -sf test/glb/namelist.configure.in .
ln -sf test/glb/namelist.test .


driver=regrid_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &regrid_lst
    file_base_2d_t="../../linkdir/mxe/glb/snp/hs_sfc_snp_tideh",
  /
EOF

exit 0
