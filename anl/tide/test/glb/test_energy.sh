#!/bin/bash


ln -sf test/glb/namelist.configure.in .
ln -sf test/glb/namelist.test .

dir=../../linkdir/mxe/glb
file_base_ssh=${dir}/snp/hs_sfc_snp_tideh
file_base_um=${dir}/snp/hs_sfc_snp_tideu
file_base_vm=${dir}/snp/hs_sfc_snp_tidev
file_base_tidept=${dir}/snp/hs_tidept_snp_h


driver=energy_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &energy_lst
    file_base_ssh="${file_base_ssh}",
    file_base_um="${file_base_um}",
    file_base_vm="${file_base_vm}",
    file_base_tidept="${file_base_tidept}",
    alpha_tide=0.88,
    beta_tide=0.7,
    nstr=1,
    nend=1,
  /
EOF

exit 0
