#!/bin/bash


ln -sf test/glb/namelist.test .
ln -sf test/glb/namelist.configure.in .


driver=mean_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &mean_lst
    file_base="../../linkdir/mxe/glb/snp/hs_snp_ssh",
    fileo="ssh_mean",
    l2d=.true.,
    cgrid="T",
    operate="square",
  /
EOF

exit 0
