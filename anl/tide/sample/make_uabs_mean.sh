#!/bin/bash


if [ ! -d sample ]; then
  cd ..
fi

. ../../setting/set_environment.sh

ln -sf test/namelist.configure.in.glb namelist.configure.in

file_base_ssh=${rootpath_data}/tide/glb/hs_sfc_snp_tideh
file_base_um=${rootpath_data}/tide/glb/hs_sfc_snp_tideu
file_base_vm=${rootpath_data}/tide/glb/hs_sfc_snp_tidev
nstr=1
nend=1


exe=uabs_mean_ctl
make ${exe}

./${exe}<<EOF
  &uabs_mean_lst
    file_base_ssh="${file_base_ssh}",
    file_base_um="${file_base_um}",
    file_base_vm="${file_base_vm}",
    nstr=${nstr},
    nend=${nend},
  /
EOF

exit 0
