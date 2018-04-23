#!/bin/bash


ln -sf test/glb/namelist.configure.in .
ln -sf test/glb/namelist.test.K1 namelist.test


driver=tidehmap_K1_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &tidehmap_lst
    diro="../../linkdir/mxe/temp",
    ltopo=.true.,
  /
EOF

exit 0
