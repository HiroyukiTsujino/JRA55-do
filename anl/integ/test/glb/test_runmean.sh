#!/bin/bash


ln -sf test/glb/namelist.test .
ln -sf test/glb/namelist.configure.in.2 namelist.configure.in


driver=runmean_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &runmean_lst
    file_base="../../linkdir/mxe/glb/snp/hs_snp_ssh",
    diro="../../linkdir/mxe/temp",
    fileo="ssh_runmean",
    l2d=.true.,
    cgrid="T",
    width_unit="record",
    width=3,
  /
EOF

exit 0
