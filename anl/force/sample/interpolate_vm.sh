#!/bin/bash

set -e

ln -sf test/seto/NAMELIST.MXE .

driver=interpolate_ctl
make ${driver}
./${driver} <<EOF
&nml_force_data
  file_data      = "../../linkdir/mxe/wnp/data/vm_Decloedt_wnp.dat",
  file_data_grid = "../../linkdir/mxe/wnp/data/vm_Decloedt_wnp_grid.d",
  im             = 1137,
  jm             = 534,
  km             = 51,
  num_elm        = 2,
  interval_ical  =    0, 1, 0, 0, 0, 0,
  first_ical     = 2001, 1, 1, 0, 0, 0,
  last_ical      = 2001, 1, 1, 0, 0, 0,
/
&nml_interpolate
  file_base_out  = "vm_Decloedt_seto.dat",
  file_remap     = "../../linkdir/mxe/seto/data/remap-vm_Decloedt_wnp.dat",
  n_elm          = 2,
/
EOF

rm NAMELIST.MXE

exit 0
