#!/bin/bash

ln -sf test/glb/namelist.test.2 namelist.test

dir=../../linkdir/mxe/glb
file_namelist1=test/glb/namelist.configure.in
file_namelist2=test/glb/namelist.configure.in
file_base1=${dir}/snp/hs_sfc_snp_um
file_base2=${dir}/snp/hs_sfc_snp_um.2
l2d=.true.
cgrid=U


driver=diff_mean_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &diff_mean_lst
    file_namelist1="${file_namelist1}",
    file_namelist2="${file_namelist2}",
    file_base1="${file_base1}",
    file_base2="${file_base2}",
    fileo="${fileo}",
    l2d=${l2d},
    cgrid="${cgrid}",
    operate="square",
  /
EOF

exit 0
