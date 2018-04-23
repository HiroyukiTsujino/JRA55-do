#!/bin/bash


dir=../../linkdir/mxe/modis
file_base=${dir}/A2GL11105310443OD1_OSTAQ_01000_01000_sst
fileo=sst.20110531

exe=snap_ctl
make ${exe}

./${exe}<<EOF
  &snap_lst
    file_base="${file_base}",
    fileo="${fileo}",
    lin= .false.,.false.,.false.,.false., .false.,.true.,.false.,.false., .false.,.false.,.false.,.false., 
    mx_str=1,
    mx_end=2,
    my_str=1,
    my_end=2,
    lgrads=.true.,
  /
EOF

exit 0
