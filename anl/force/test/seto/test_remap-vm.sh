#!/bin/bash

set -e

ln -sf test/seto/namelist.test-2 namelist.test
ln -sf test/seto/NAMELIST.MXE .

driver=remap_driver
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
&nml_remap
  undef          = 0.e0,
/
EOF

rm NAMELIST.MXE

exit 0
