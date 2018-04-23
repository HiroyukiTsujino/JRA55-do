#!/bin/bash


file_base=/worke/htsujino/NOCS/grads_ann/latent
fileo=latent_glb
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
