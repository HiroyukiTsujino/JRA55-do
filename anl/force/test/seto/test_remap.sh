#!/bin/bash

set -e

ln -sf test/seto/namelist.test .
ln -sf test/seto/NAMELIST.MXE .

driver=remap_driver
make ${driver}
./${driver} <<EOF
&nml_force_data
  file_data      = "../../linkdir/mxe/move-wnp/data/hfcomp_6hr_gsm.y20110101",
  file_data_grid = "../../linkdir/mxe/move-wnp/data/gsm_grid.dat",
  im             = 997,
  jm             = 439,
  num_elm        = 4,
  interval_ical  =    0, 0, 0, 6, 0, 0,
  first_ical     = 2010,12,31,21, 0, 0,
  last_ical      = 2011, 1, 1,21, 0, 0,
/
EOF

rm NAMELIST.MXE

exit 0
