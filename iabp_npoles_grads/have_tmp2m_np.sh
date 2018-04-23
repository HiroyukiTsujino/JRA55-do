#!/bin/bash

file_base=/worke/htsujino/IABP_NPOLES/grads/tair
fileo=tmp2m_70N
file_mask=/worke/htsujino/COBESST/data/np_mask.gd
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
