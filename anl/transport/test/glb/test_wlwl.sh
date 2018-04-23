#!/bin/bash

set -e

ln -sf test/glb/NAMELIST.MXE .
#ln -sf test/glb/namelist.configure.in .
ln -sf test/glb/namelist.test .

dir=../../linkdir/mxe/glb

driver=wlwl_driver
make ${driver}

./${driver}<<EOF
  &wlwl_lst
    file_base_ssh = "${dir}/snp/hs_snp_ssh",
    file_base_u   = "${dir}/snp/hs_snp_u",
    file_base_v   = "${dir}/snp/hs_snp_v",
    diro          = "../../linkdir/mxe/temp",
    lw2           = .true.,
  /
EOF

rm NAMELIST.MXE

exit 0
