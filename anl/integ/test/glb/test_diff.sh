#!/bin/bash


ln -sf test/glb/namelist.test .

file_namelist1=test/glb/namelist.configure.in
file_namelist2=test/glb/namelist.configure.in

dir=../../linkdir/mxe/glb
file_base1=${dir}/snp/hs_snp_ssh
file_base2=${dir}/snp/hs_snp_ssh.2
diro=../../linkdir/mxe/temp
fileo=diff_ssh
l2d=.true.
cgrid=T


driver=diff_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &diff_lst
    file_namelist1="${file_namelist1}",
    file_namelist2="${file_namelist2}",
    file_base1="${file_base1}",
    file_base2="${file_base2}",
    diro="${diro}",
    fileo="${fileo}",
    l2d=${l2d},
    cgrid="${cgrid}",
  /
EOF

exit 0
