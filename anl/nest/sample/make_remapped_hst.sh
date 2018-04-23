#!/bin/bash


exe=remap_ctl
make ${exe} || exit 1

./${exe}<<EOF
&nml_remap
  file_configure_src="test/rectangle/namelist.configure-sub.in",
  file_configure_out="test/rectangle/namelist.configure.in",
  file_base_src="../../linkdir/mxe/rect/hst-sub/hs_u",
  dir_out="../../linkdir/mxe/temp",
  file_base_out="hs_u",
  file_remap="../../linkdir/mxe/rect/data-sub/rmp_c2pu_aave_3d.d",
  cgrid="U",
  nrec_first=1,
  nrec_last=1,
/
EOF

exit 0
