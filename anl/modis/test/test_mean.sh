#!/bin/bash


ln -sf test/namelist.test .


driver=mean_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &mean_lst
    file_list="test/file_list.txt",
    fileo="dummy",
    mx_str=1,
    mx_end=2,
    my_str=1,
    my_end=2,
  /
EOF

exit 0
