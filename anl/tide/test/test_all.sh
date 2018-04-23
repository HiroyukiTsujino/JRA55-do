#!/bin/bash

sh test/glb/test_regrid.sh > log.regrid.txt || exit 1
sh test/glb/test_gradient.sh > log.gradient.txt || exit 1
sh test/glb/test_jcoast.sh > log.jcoast.txt || exit 1
sh test/glb/test_energy.sh > log.energy.txt || exit 1
sh test/glb/test_uabs_mean.sh > log.uabs_mean.txt || exit 1
sh test/glb/test_btro_vector.sh > log.btro_vector.txt || exit 1
sh test/jpn/test_map.sh > log.map.txt || exit 1
sh test/jpn/test_cap_jcoast.sh > log.cap_jcoast.txt || exit 1

sh test/check_log.sh || exit 1

exit 0
