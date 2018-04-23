#!/bin/bash

set -e

file_base=../linkdir/CORE/core_cobesst_annual_dec2015/nswrf

l2d=.true.
cgrid=U

exe=have_ctl

for region in wtp2
do
fileo=nswrf_${region}_dec2015
file_mask=/work116/htsujino/COBESST/data/${region}_mask.gd
./${exe}<<EOF
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=${l2d},
    cgrid="${cgrid}",
    file_mask="${file_mask}"
  /
EOF
done

exit 0
