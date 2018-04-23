#!/bin/bash

set -e

sh test/rect/test_trim_hs.sh > log.trim_hs.txt

sh test/check_log.sh

exit 0
