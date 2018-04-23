#!/bin/bash


ln -sf test/glb/namelist.test .
ln -sf test/glb/namelist.configure.in .

dir=../../linkdir/mxe/glb
file_base=${dir}/snp/hs_snp_ssh
fileo=ssh_rms

driver=have_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=.true.,
    cgrid="T",
    operate="square",
  /
EOF


exit 0
