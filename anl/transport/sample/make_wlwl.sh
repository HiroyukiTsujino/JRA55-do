#!/bin/bash


. ../../setting/set_environment.sh


ln -sf test/namelist.configure.in.glb namelist.configure.in

file_base_ssh=${rootpath_data}/transport/glb/hs_snp_ssh
file_base_u=${rootpath_data}/transport/glb/hs_snp_u
file_base_v=${rootpath_data}/transport/glb/hs_snp_v
diro=${rootpath_data}/temp
lw2=.true.


exe=wlwl_ctl
make ${exe}

./${exe}<<EOF
  &wlwl_lst
    file_base_ssh="${file_base_ssh}",
    file_base_u="${file_base_u}",
    file_base_v="${file_base_v}",
    diro="${diro}",
    lw2=${lw2},
  /
EOF

exit 0
