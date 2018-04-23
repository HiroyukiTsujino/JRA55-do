#!/bin/bash

exe=zave_ctl

./${exe}<<EOF
&zave_lst
  file_base="/worke/htsujino/NOCS/grads/latent",
  fileo_base="/worke/htsujino/NOCS/grads/latent_zm"
  l2d=.true.,
  cgrid="U",
  file_mask="/worke/htsujino/COBESST/data/cobesst-mask.gd"
/
EOF

exit 0
