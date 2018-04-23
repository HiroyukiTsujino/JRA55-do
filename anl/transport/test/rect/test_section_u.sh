#!/bin/bash

set -e

ln -sf test/rect/NAMELIST.MXE .
ln -sf test/rect/namelist.test .

dir=../../linkdir/mxe/rect/hst

driver=section_u_driver
make ${driver}
./${driver} <<EOF
&nml_section_u
  file_base_ssh = "${dir}/hs_ssh",
  file_base_u   = "${dir}/hs_u",
  file_out      = "dummy",
  l_zonal       = .true.,
  i_first       = 1,
  i_last        = 62,
  j_section     = 20,
/
EOF

rm NAMELIST.MXE

exit 0
