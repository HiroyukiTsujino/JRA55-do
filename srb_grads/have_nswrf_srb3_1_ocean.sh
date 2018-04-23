#!/bin/bash

file_base=/worke/htsujino/SRB/rel3.1/grads_ann/nswrf
fileo=nswrf_srb3_1

l2d=.true.
cgrid=U

exe=have_ctl

./${exe}<<EOF
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=${l2d},
    cgrid="${cgrid}",
  /
EOF

exit 0
