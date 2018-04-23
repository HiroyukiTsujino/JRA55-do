#!/bin/bash

sh test/test_density.sh > log.density.txt || exit 1
sh test/glb/test_ts2sigma.sh > log.ts2sigma.txt || exit 1

sh test/check_log.sh || exit 1

exit 0
