#!/bin/bash

extin=${1}
extgd=${2}
nhist=${3}

if [ x${nhist} = x ]; then
  echo " Usage : mkhist_ocean.sh inputfile_extension outputfile_extension number_of_data "
  exit
fi

#####indir=/work1/htsujino/trp1x05L51bbl/result-0414
######outdir=/work1/htsujino/trp1x05L51bbl/result-0414

indir=../run/result
outdir=../run/result
datadir=../data

######

file_grid=${datadir}/vgrid.d
file_topo=${datadir}/topo.d

linearx=.true.
lineary=.false.
linearz=.false.

# input file

split=.false.
file_in=${indir}/hist.${extin}
file_in_u=${indir}/u.${extin}
file_in_v=${indir}/v.${extin}
file_in_t=${indir}/t.${extin}
file_in_s=${indir}/s.${extin}
file_in_h=${indir}/h.${extin}

# output file

grads_file_u=${outdir}/hs_u.${extgd}
grads_file_v=${outdir}/hs_v.${extgd}
grads_file_t=${outdir}/hs_t.${extgd}
grads_file_s=${outdir}/hs_s.${extgd}
grads_file_h=${outdir}/hs_ssh.${extgd}

# GrADs CTL file

ctl_true_or_false=.true.
#ctl_true_or_false=.false.
grads_ctl_file_u=./hs_u.ctl
grads_ctl_file_v=./hs_v.ctl
grads_ctl_file_t=./hs_t.ctl
grads_ctl_file_s=./hs_s.ctl
grads_ctl_file_h=./hs_ssh.ctl

./mkgrads_ocean_hist <<EOF
 &nocean
  file_grid="${file_grid}",
  file_topo="${file_topo}",
  lsplit=${split}
  file_in="${file_in}",
  file_in_u="${file_in_u}",
  file_in_v="${file_in_v}",
  file_in_t="${file_in_t}",
  file_in_s="${file_in_s}",
  file_in_h="${file_in_h}",
  file_out_u="${grads_file_u}",
  file_out_v="${grads_file_v}",
  file_out_t="${grads_file_t}",
  file_out_s="${grads_file_s}",
  file_out_h="${grads_file_h}",
  file_ctl_u="${grads_ctl_file_u}",
  file_ctl_v="${grads_ctl_file_v}",
  file_ctl_t="${grads_ctl_file_t}",
  file_ctl_s="${grads_ctl_file_s}",
  file_ctl_h="${grads_ctl_file_h}",
  inum=${nhist},
  undef=-9.99e33,
  lmakectl=${ctl_true_or_false},
  linear_x=${linearx},
  linear_y=${lineary},
  linear_z=${linearz}
 /
EOF
