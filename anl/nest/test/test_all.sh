#!/bin/bash

sh test/rectangle/test_remap.sh > log.remap.txt || exit 1

sh test/check_log.sh || exit 1

exit 0

