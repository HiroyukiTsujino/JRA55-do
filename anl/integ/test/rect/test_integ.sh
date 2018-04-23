#!/bin/bash

set -e

ln -sf test/rect/NAMELIST.MXE .
ln -sf test/rect/namelist.test .

dir=../../linkdir/mxe/rect/hst

driver=integ_driver
make ${driver}
./${driver} <<EOF
  &integ_lst
    file_base_2d_t = "${dir}/hs_ssh",
    file_base_2d_u = "${dir}/hs_sfc_um",
    file_base_3d_t = "${dir}/hs_t",
    file_base_3d_u = "${dir}/hs_u",
    file_base_ssh  = "${dir}/hs_ssh",
  /
EOF

rm NAMELIST.MXE

exit 0
