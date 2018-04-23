#!/bin/bash


ln -sf test/glb/namelist.test .
ln -sf test/glb/namelist.configure.in .


driver=libmxe_para_driver
make ${driver} || exit 1

./${driver} || exit 1

exit 0
