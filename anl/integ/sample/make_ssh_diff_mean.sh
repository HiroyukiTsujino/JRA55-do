#!/bin/bash


if [ ! -d sample ]; then
  cd ..
fi

. ../../setting/test/set_environment.sh


ln -sf test/namelist.test.glb namelist.test

file_namelist1=test/namelist.configure.in.glb
file_namelist2=test/namelist.configure.in.glb
file_base1=${rootpath_data}/integ/hs_snp_ssh.glb
file_base2=${rootpath_data}/integ/hs_snp_ssh.glb.2
fileo=ssh_diff_mean
l2d=.true.
cgrid=T


exe=diff_mean_ctl
make ${exe}

./${exe}<<EOF
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
