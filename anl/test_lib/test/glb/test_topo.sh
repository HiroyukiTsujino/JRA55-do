#!/bin/bash


ln -sf test/glb/namelist.test .
ln -sf test/glb/namelist.configure.in .

driver=libmxe_topo_driver
make ${driver} || exit 1

./${driver} || exit 1

exit 0
