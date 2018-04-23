#!/bin/bash


ln -sf test/glb/namelist.configure.in .
ln -sf test/glb/namelist.test .

dir=../../linkdir/mxe/glb

driver=uabs_mean_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &uabs_mean_lst
    file_base_ssh="${dir}/snp/hs_sfc_snp_tideh",
    file_base_um="${dir}/snp/hs_sfc_snp_tideu",
    file_base_vm="${dir}/snp/hs_sfc_snp_tidev",
    nstr=1,
    nend=1,
  /
EOF

exit 0
