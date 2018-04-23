#!/bin/bash


if [ ! -d sample ]; then
  cd ..
fi
. ../../setting/set_environment.sh

ln -sf sample/namelist.configure.in.glb namelist.configure.in

file_base=${rootpath_data}/integ/hs_snp_ssh.glb
fileo=ssh_rms
l2d=.true.
cgrid=T


exe=have_ctl
make ${exe}

./${exe}<<EOF
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=${l2d},
    cgrid="${cgrid}",
  /
EOF


exit 0

