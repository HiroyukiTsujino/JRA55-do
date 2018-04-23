#!/bin/bash


ln -sf test/glb/namelist.test .
ln -sf test/glb/namelist.configure.in .

dir=../../linkdir/mxe/glb

driver=integ_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &integ_lst
    file_base_2d_t="${dir}/snp/hs_snp_ssh",
    file_base_2d_u="${dir}/snp/hs_sfc_snp_um",
    file_base_3d_t="${dir}/snp/hs_t",
    file_base_3d_u="${dir}/snp/hs_snp_u",
  /
EOF

exit 0
