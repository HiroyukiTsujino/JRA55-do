#!/bin/bash

file_base=/workd/htsujino/OAflux/grads_annual/sph2m
fileo=sph2m

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
