#!/bin/bash


ln -sf test/glb/namelist.configure.in .
ln -sf test/glb/namelist.test .


driver=spot_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &spot_lst
    lon=140.d0,
    lat=20.d0,
    fileo="dummy",
  /
EOF

exit 0
