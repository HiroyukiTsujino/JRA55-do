#!/bin/bash

ln -sf test/rect/namelist.configure.in .

driver=power_ctl
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &nml_power
    file_base_in = "../../linkdir/mxe/rect/hst/hs_ssh",
    file_base_out = "dummy",
    calc_i = 20,
    calc_j = 10,
    l2d = .true.,
  /
EOF

exit 0
