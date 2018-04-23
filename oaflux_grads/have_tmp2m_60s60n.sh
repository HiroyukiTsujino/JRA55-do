#!/bin/bash

file_base=/workd/htsujino/OAflux/grads_annual/tmp2m
fileo=tmp2m_60S60N
file_mask=/worke/htsujino/COBESST/data/60s60n_mask.gd
l2d=.true.
cgrid=U

exe=have_ctl

./${exe}<<EOF
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=${l2d},
    cgrid="${cgrid}",
    file_mask="${file_mask}"
  /
EOF

exit 0
