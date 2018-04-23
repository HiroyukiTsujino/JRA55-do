#!/bin/bash


ln -sf test/jpn/namelist.configure.in .
ln -sf test/jpn/namelist.test .


driver=map_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &map_test_lst
    lon=131.d0,
    lat=33.5d0,
  /
EOF

exit 0
