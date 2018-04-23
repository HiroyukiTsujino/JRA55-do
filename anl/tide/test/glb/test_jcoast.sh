#!/bin/bash


ln -sf test/glb/namelist.test .


driver=jcoast_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &jcoast_lst
    filei="../../linkdir/mxe/data/tide/japan_coast/NAGASAKI.2001",
  /
EOF

exit 0
