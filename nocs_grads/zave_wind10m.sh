#!/bin/bash

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="/worke/htsujino/NOCS/grads/wind10m",
  fileo_base="/worke/htsujino/NOCS/grads/wind10m_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/worke/htsujino/COBESST/data/cobesst-mask.gd"
/
EOF

exit 0
