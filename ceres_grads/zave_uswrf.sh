#!/bin/bash

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="/worke/htsujino/CERES/grads/swup",
  fileo_base="/worke/htsujino/CERES/grads/swup_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/worke/htsujino/COBESST/data/cobe_jra-mask.gd"
/
EOF

exit 0
