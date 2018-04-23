#!/bin/bash

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="/worke/htsujino/SRB/rel3.1/grads/swdn",
  fileo_base="/worke/htsujino/SRB/rel3.1/grads/swdn_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/worke/htsujino/COBESST/data/cobesst-mask.gd"
/
EOF

exit 0
