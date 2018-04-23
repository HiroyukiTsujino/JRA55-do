#!/bin/bash

extin=${1}
extgd=${2}
nrst=${3}

if [ x${extgd} = x ]; then
  echo " Usage : mkrestart_ice.sh inputfile_extension outputfile_extension number_of_data "
  exit
fi

#indir=/work1/htsujino/trp1x05L51bbl/result-0414
#outdir=/work1/htsujino/trp1x05L51bbl/result-0414

indir=../result
outdir=../grddat

datadir=../data

# define grid 

file_grid=${datadir}/vgrid.d
file_topo=${datadir}/topo.d

linearx=.true.
lineary=.false.
linearz=.false.

file_ice=${indir}/ice_restart.${extin}
grads_ice=${outdir}/rs_ice.${extgd}

echo "${file_ice}"

ctl_true_or_false=.true.
#ctl_true_or_false=.false.
#fname_ctl_ts=${outdir}/rs_ice_ts.ctl
#fname_ctl_uv=${outdir}/rs_ice_uv.ctl

fname_ctl_ts=./rs_ice_ts.ctl
fname_ctl_uv=./rs_ice_uv.ctl

./mkgrads_ice_restart <<EOF
 &nicerst
  file_grid="${file_grid}",
  file_topo="${file_topo}",
  file_in="${file_ice}",
  file_out="${grads_ice}",
  file_ctl_ts="${fname_ctl_ts}",
  file_ctl_uv="${fname_ctl_uv}",
  inum=${nrst},
  lmakectl=${ctl_true_or_false},
  linear_x=${linearx},
  linear_y=${lineary},
  linear_z=${linearz}
 /
EOF
