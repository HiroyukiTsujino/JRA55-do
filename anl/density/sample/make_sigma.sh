#!/bin/bash


ln -sf test/glb/namelist.configure.in .

exe=ts2sigma_ctl
make ${exe}

./${exe}<<EOF
  &ts2sigma_lst
    file_base_t="../../linkdir/mxe/glb/hst/hs_t",
    file_base_s="../../linkdir/mxe/glb/hst/hs_s",
    dir_out="../../linkdir/mxe/temp",
    file_out="hs_sigma0",
  /
EOF

exit 0
