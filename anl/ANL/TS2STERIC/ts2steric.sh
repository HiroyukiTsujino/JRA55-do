#!/bin/bash

yyyymm=${1}

./ts2steric<<EOF
 &nml_steric
  flin_t="../result/hs_t.${yyyymm}",
  flin_s="../result/hs_s.${yyyymm}",
  flin_ssh="../result/hs_ssh.${yyyymm}",
  flin_tsclim="../../data/tsclim.d",
  flout0="../logs/height.txt",
  flout1="../logs/steric_sealevel_change.${yyyymm}",
  flout2="../logs/thermosteric_sealevel_change.${yyyymm}",
  flout3="../logs/model_sealevel.${yyyymm}",
  flmaskt="../../data/maskt.gd",
  ftopo="../../data/topo.d",
  fgrid="../../data/vgrid.d",
  fscale="../../data/scale_factor.d",
 /
EOF
