#!/bin/bash -f

set -e

rm -f namelist.spector

for filter in 9p20 9p50
do
  ln -sf namelist.spector.${filter} namelist.spector
  ./wave_number_fft > wave_number_fft_${filter}.log
done
