#!/bin/bash
#- Barotropic transport UM (vertical integrated u)

ln -sf test/glb/namelist.configure.in-hst namelist.configure.in

dir=../../linkdir/mxe/glb/hst


driver=integ_vert_ctl
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &nml_integ_vert
    file_base_in="${dir}/hs_u",
    file_base_ssh="${dir}/hs_ssh",
    dir_out="../../linkdir/mxe/temp",
    file_base_out="um",
    cgrid="U",
  /
EOF


exit 0
