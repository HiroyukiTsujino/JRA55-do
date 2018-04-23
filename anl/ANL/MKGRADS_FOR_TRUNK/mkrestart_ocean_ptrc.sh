#!/bin/bash

extin=${1}
extgd=${2}
nrst=${3}

if [ x${nrst} = x ]; then
  echo " Usage : mkrestart_ocean_ptrc.sh inputfile_extension outputfile_extension number_of_data "
  exit
fi

#####indir=/work1/htsujino/trp1x05L51bbl/result-0414
######outdir=/work1/htsujino/trp1x05L51bbl/result-0414

indir=../run-mip-20120203/restart
outdir=../run-mip-20120203/restart
datadir=../data

######

file_grid=${datadir}/vgrid.d
file_topo=${datadir}/topo.d

linearx=.true.
lineary=.false.
linearz=.false.

# input file

split=.false.
file_in=${indir}/rst_iage.${extin}

# output file

grads_file=${outdir}/rs_iage.${extgd}

# GrADs CTL file

ctl_true_or_false=.true.
#ctl_true_or_false=.false.
grads_ctl_file=./rs_iage.ctl

./mkgrads_ocean_restart_ptrc <<EOF
 &nocrst
  file_grid="${file_grid}",
  file_topo="${file_topo}",
  file_in="${file_in}",
  file_out="${grads_file}",
  file_ctl="${grads_ctl_file}",
  inum=${nrst},
  undef=0.0e1,
  lmakectl=${ctl_true_or_false},
  linear_x=${linearx},
  linear_y=${lineary},
  linear_z=${linearz}
 /
EOF
