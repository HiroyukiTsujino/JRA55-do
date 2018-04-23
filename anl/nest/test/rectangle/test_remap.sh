#!/bin/bash


ln -sf test/rectangle/namelist.test .

driver=remap_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
&nml_remap
  file_configure_src="test/rectangle/namelist.configure-sub.in",
  file_configure_out="test/rectangle/namelist.configure.in",
  file_base_src="../../linkdir/mxe/rect/hst-sub/hs_u",
  dir_out="../../linkdir/mxe/temp",
  file_base_out="dummy",
  file_remap="../../linkdir/mxe/rect/data-sub/rmp_c2pu_aave_3d.d",
  cgrid="U",
/
EOF

exit 0
