#!/bin/bash


ln -sf test/glb/namelist.configure.in .
ln -sf test/glb/namelist.test .


driver=tidehmap_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &tidehmap_lst
    diro="../../linkdir/mxe/temp",
    ltopo=.true.,
  /
EOF

exit 0
