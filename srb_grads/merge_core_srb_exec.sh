#!/bin/bash

itemc=${1}
items=${2}

for mon in 01 02 03 04 05 06 07 08 09 10 11 12
do
  echo ${mon}
  sed -e s/@mon@/${mon}/ \
      -e s/@itemc@/${itemc}/ \
      -e s/@items@/${items}/ \
      namelist.merge_core_srb_template > namelist.merge_core_srb
  ./merge_core_srb
done
