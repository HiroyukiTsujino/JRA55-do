#!/bin/bash


ln -sf test/glb/namelist.configure.in .
ln -sf test/glb/namelist.test .

dir=../../linkdir/mxe/glb
file_base_2d_t=${dir}/snp/hs_sfc_snp_tideh
file_base_2d_u=${dir}/snp/hs_sfc_snp_tideu


driver=gradient_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &gradient_lst
    file_base_2d_t="${file_base_2d_t}",
    file_base_2d_u="${file_base_2d_u}",
  /
EOF

exit 0
