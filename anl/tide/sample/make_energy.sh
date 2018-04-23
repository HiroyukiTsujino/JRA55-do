#!/bin/bash


if [ ! -d sample ]; then
  cd ..
fi

. ../../setting/set_environment.sh


ln -sf test/namelist.configure.in.glb namelist.configure.in

file_base_ssh=${rootpath_data}/tide/glb/hs_sfc_snp_tideh
file_base_um=${rootpath_data}/tide/glb/hs_sfc_snp_tideu
file_base_vm=${rootpath_data}/tide/glb/hs_sfc_snp_tidev
file_base_tidept=${rootpath_data}/tide/glb/hs_tidept_snp_h
alpha_tide=0.88
beta_tide=0.7
nstr=1
nend=1

exe=energy_ctl
make ${exe}

./${exe}<<EOF
  &energy_lst
    file_base_ssh="${file_base_ssh}",
    file_base_um="${file_base_um}",
    file_base_vm="${file_base_vm}",
    file_base_tidept="${file_base_tidept}",
    alpha_tide=${alphat_tide},
    beta_tide=${beta_tide},
    nstr=${nstr},
    nend=${nend},
  /
EOF

exit 0
