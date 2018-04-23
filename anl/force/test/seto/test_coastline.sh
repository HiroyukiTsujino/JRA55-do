#!/bin/bash

set -e

ln -sf test/seto/namelist.test .
ln -sf test/seto/NAMELIST.MXE .

driver=coastline_driver
gmake ${driver}
./${driver} <<EOF
&nml_seek_nearest_sea
  lon            = 130.d0,
  lat            = 32.d0,
/
&nml_seek_nearest_sea
  lon            = 130.d0,
  lat            = 32.d0,
/
&nml_seek_nearest_sea
  lon            = 132.d0,
  lat            = 32.d0,
/
EOF

rm NAMELIST.MXE

exit 0
