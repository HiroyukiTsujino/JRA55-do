#!/bin/bash

if [ $# -lt 1 ]; then
  echo Usage: sh $0 EXP
  exit 1
fi
exp=$1

diri=../../linkdir/result/${exp}

file_base_u=${diri}/hs_sq_u
file_base_v=${diri}/hs_sq_v
file_base_ssh=${diri}/hs_ssh

prog=vave_ctl
make ${prog}

./${prog}<<EOF
  &vave_lst
    file_base="${file_base_u}",
    file_base_ssh="${file_base_ssh}",
    fileo="sq_u_vave",
    cgrid="U",
  /
EOF

./${prog}<<EOF
  &vave_lst
    file_base="${file_base_v}",
    file_base_ssh="${file_base_ssh}",
    fileo="sq_v_vave",
    cgrid="U",
  /
EOF


exit 0
