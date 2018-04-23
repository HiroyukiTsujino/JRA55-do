#!/bin/bash


ln -sf test/glb/namelist.test .
ln -sf test/glb/namelist.configure.in .

dir=../../linkdir/mxe/glb


driver=vave_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &vave_lst
    file_base="${dir}/snp/hs_snp_u",
    file_base_ssh="${dir}/snp/hs_snp_ssh",
    fileo="u_vave",
    cgrid="U",
  /
EOF


exit 0
