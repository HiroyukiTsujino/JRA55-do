#!/bin/bash


ln -sf test/glb/namelist.test .
ln -sf test/glb/namelist.configure.in-hst namelist.configure.in

dir=../../linkdir/mxe/glb/hst


driver=integ_vert_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &nml_integ_vert
    file_base_in="${dir}/hs_t",
    file_base_ssh="${dir}/hs_ssh",
    dir_out="${dir}",
    file_base_out="dummy",
    cgrid="T",
  /
EOF


exit 0
