#!/bin/bash

file_base=/worke/htsujino/IABP_NPOLES/grads/tair
file_base_ice=/workc/ocpublic/refdata/COBE-SST/monthly/yyyymm/ice-glb
fileo=tmp2m_70N
file_mask=/worke/htsujino/COBESST/data/np_mask.gd
cgrid=U

exe=have_on_ice_ctl

./${exe}<<EOF
  &have_ice_lst
    file_base="${file_base}",
    file_base_ice="${file_base_ice}",
    fileo="${fileo}",
    cgrid="${cgrid}",
    file_mask="${file_mask}"
    def_ice=0.5d0
  /
EOF

exit 0
