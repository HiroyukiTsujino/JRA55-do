#!/bin/bash

set -e

rm -f NAMELIST.MXE
ln -sf test/rect/namelist.test .
ln -sf test/rect/namelist.configure.in .

driver=libmxe_topo_driver
make ${driver}

./${driver}

exit 0
