#!/bin/bash


ln -sf test/glb/namelist.test.3 namelist.test
ln -sf test/glb/namelist.configure.in .

dir=../../linkdir/mxe/glb
file_base=${dir}/snp/hs_snp_ssh
fileo=ssh_rms
file_mask=${dir}/data/maskt.gd

driver=have_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=.true.,
    cgrid="T",
    file_mask="${file_mask}",
  /
EOF


exit 0
