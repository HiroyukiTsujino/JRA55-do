#!/bin/bash

set -e

sh test/glb/test_wlwl.sh > log.wlwl.txt

sh test/check_log.sh

exit 0

