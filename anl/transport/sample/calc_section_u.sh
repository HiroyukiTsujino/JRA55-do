#!/bin/bash

set -e

ln -sf test/rect/NAMELIST.MXE .

dir=../../linkdir/mxe/rect/hst

driver=section_u_ctl
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
  nrec_first    = 1,
  nrec_last     = 1,
/
EOF

exit 0
