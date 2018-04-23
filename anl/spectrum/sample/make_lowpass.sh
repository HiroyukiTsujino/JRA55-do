#!/bin/bash

ln -sf test/rect/namelist.configure.in .

driver=lowpass_ctl
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &nml_lowpass
    file_base_in = "../../linkdir/mxe/rect/hst/hs_ssh",
    file_base_out = "lowpass",
    calc_i = 20,
    calc_j = 10,
    l2d = .true.,
    nfreq_max = 1 ,
  /
EOF

exit 0
