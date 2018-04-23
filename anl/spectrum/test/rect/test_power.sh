#!/bin/bash

set -e

ln -sf test/rect/namelist.test .
ln -sf test/rect/namelist.configure.in .

driver=power_driver
make ${driver}

./${driver}<<EOF
  &nml_power
    file_base_in = "../../linkdir/mxe/rect/hst/hs_ssh",
    file_base_out = "dummy",
    calc_i = 20,
    calc_j = 10,
    l2d = .true.,
  /
EOF

exit 0
