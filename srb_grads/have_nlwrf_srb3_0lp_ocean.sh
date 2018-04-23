#!/bin/bash

file_base=/worke/htsujino/SRB/rel3.0_lpsa_lpla/grads_ann/nlwrf
fileo=nlwrf_srb3_0lp

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
