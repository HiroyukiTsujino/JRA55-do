#!/bin/bash -f

set -e

for mon in 01 02 03 04 05 06 07 08 09 10 11 12
do
  sed -e s%@mon@%${mon}% \
  namelist.core_to_cama_green_template > namelist.core_to_cama
  ./core_to_cama
done
