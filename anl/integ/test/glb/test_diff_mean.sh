#!/bin/bash


ln -sf test/glb/namelist.test .

dir=../../linkdir/mxe/glb
file_namelist1=test/glb/namelist.configure.in
file_namelist2=test/glb/namelist.configure.in
file_base1=${dir}/snp/hs_snp_ssh
file_base2=${dir}/snp/hs_snp_ssh.2


driver=diff_mean_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &diff_mean_lst
    file_namelist1="${file_namelist1}",
    file_namelist2="${file_namelist2}",
    file_base1="${file_base1}",
    file_base2="${file_base2}",
    fileo="dummy",
    l2d=.true.,
    cgrid="T",
    operate="square",
  /
EOF

exit 0
