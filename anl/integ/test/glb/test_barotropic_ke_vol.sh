#!/bin/bash


ln -sf test/glb/namelist.test .
ln -sf test/glb/namelist.configure.in .

dir=../../linkdir/mxe/glb


driver=barotropic_ke_vol_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &nml_barotropic_ke_vol
    file_base_ssh="${dir}/snp/hs_snp_ssh",
    file_base_um="${dir}/snp/hs_sfc_snp_tideu",
    file_base_vm="${dir}/snp/hs_sfc_snp_tidev",
    file_base_out="barotropic_ke_vol",
    nrec_first=1,
    nrec_last=1,
  /
EOF


exit 0
