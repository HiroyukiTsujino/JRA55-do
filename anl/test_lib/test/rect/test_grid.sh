#!/bin/bash

set -e 

ln -sf test/rect/namelist.test .
ln -sf test/rect/NAMELIST.MXE .

driver=libmxe_grid_driver
make ${driver}

./${driver}

exit 0
