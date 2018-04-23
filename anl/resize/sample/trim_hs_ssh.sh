#!/bin/bash

set -e

ln -sf test/rect/namelist.configure.in .

driver=trim_hs_ctl
make ${driver}

./${driver}<<EOF
  &nml_trim_hs
    file_base_in  = "../../linkdir/mxe/rect/hst/hs_ssh",
    dir_out       = "../../linkdir/mxe/temp",
    file_base_out = "hs_ssh",
    l2d           = .true.,
    cgrid         = "T",
    i_first       = 20,
    i_last        = 40,
    j_first       = 20,
    j_last        = 40,
  /
EOF

exit 0
