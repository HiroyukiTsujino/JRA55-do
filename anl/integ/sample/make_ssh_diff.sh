#!/bin/bash


if [ ! -d sample ]; then
  cd ..
fi

. ../../setting/set_environment.sh

file_namelist1=sample/namelist.configure.in.glb
file_namelist2=sample/namelist.configure.in.glb
file_base1=${rootpath_data}/integ/hs_snp_ssh.glb
file_base2=${rootpath_data}/integ/hs_snp_ssh.glb.2
diro=${rootpath_data}/temp
fileo=diff_ssh
l2d=.true.
cgrid=T


exe=diff_ctl
make ${exe}

./${exe}<<EOF
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
