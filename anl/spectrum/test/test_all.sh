#!/bin/bash

set -e

sh test/test_fft_pgi.sh > log.fft_pgi.txt
sh test/rect/test_power.sh > log.power.txt
sh test/rect/test_lowpass.sh > log.lowpass.txt

sh test/check_log.sh

exit 0
