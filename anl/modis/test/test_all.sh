#!/bin/bash

sh test/test_snap.sh > log.snap.txt || exit 1
sh test/test_mean.sh > log.mean.txt || exit 1

sh test/check_log.sh || exit 1

exit 0
