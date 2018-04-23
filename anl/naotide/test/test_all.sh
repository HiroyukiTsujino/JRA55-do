#!/bin/bash

sh test/glb/test_tidehmap.sh > log.tidehmap.txt || exit 1
sh test/glb/test_tidehmap.K1.sh > log.tidehmap.K1.txt || exit 1
sh test/glb/test_tidehmap.M2.sh > log.tidehmap.M2.txt || exit 1
sh test/glb/test_spot.sh > log.spot.txt || exit 1

sh test/check_log.sh || exit 1

exit 0
