#!/bin/bash


ln -sf test/namelist.test .


driver=snap_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &snap_lst
    file_base="../../linkdir/mxe/modis/A2GL11105310443OD1_OSTAQ_01000_01000_sst",
    fileo="${dir}/dummy",
    lin= .false.,.false.,.false.,.false., .false.,.true.,.false.,.false., .false.,.false.,.false.,.false., 
    mx_str=1,
    mx_end=2,
    my_str=1,
    my_end=2,
  /
EOF

exit 0
