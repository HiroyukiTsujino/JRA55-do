#!/bin/bash

set -e

sh test/move-wnp/test_force_data.sh > log.force_data.txt
sh test/seto/test_remap.sh > log.remap.txt
sh test/seto/test_interpolate.sh > log.interpolate.txt
sh test/seto/test_remap-vm.sh > log.remap-2.txt
sh test/seto/test_interpolate-vm.sh > log.interpolate-2.txt
sh test/seto/test_coastline.sh > log.coastline.txt

sh test/check_log.sh

exit 0
