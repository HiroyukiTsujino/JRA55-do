#!/bin/bash

set -e

sh test/glb/test_diff.sh > log.diff.txt
sh test/glb/test_diff_mean.sh > log.diff_mean.txt
sh test/glb/test_diff_mean.2.sh > log.diff_mean.2.txt
sh test/glb/test_integ.sh > log.integ.txt
sh test/glb/test_have.sh > log.have.txt
sh test/glb/test_have.w2.sh > log.have.w2.txt
sh test/glb/test_have.2.sh > log.have.2.txt
sh test/glb/test_have.3.sh > log.have.3.txt
sh test/glb/test_mean.sh > log.mean.txt
sh test/glb/test_runmean.sh > log.runmean.txt
sh test/glb/test_vave.sh > log.vave.txt
sh test/glb/test_barotropic_ke_vol.sh > log.barotropic_ke_vol.txt
sh test/glb/test_integ_vert.sh > log.integ_vert.txt

sh test/rect/test_diff.sh > log.diff-rect.txt

sh test/check_log.sh

exit 0
