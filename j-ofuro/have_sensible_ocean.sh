#!/bin/bash

file_base=/worke/htsujino/J-OFURO/grads_annual/sensible
fileo=sensible

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
