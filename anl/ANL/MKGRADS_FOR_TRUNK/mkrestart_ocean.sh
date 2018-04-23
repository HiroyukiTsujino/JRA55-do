#!/bin/bash

extin=${1}
extgd=${2}
nrst=${3}

if [ x${nrst} = x ]; then
  echo " Usage : mkrestart_ocean.sh inputfile_extension outputfile_extension number_of_data "
  exit
fi

#####indir=/work1/htsujino/trp1x05L51bbl/result-0414
######outdir=/work1/htsujino/trp1x05L51bbl/result-0414

indir=../result
outdir=../grddat
datadir=../data

######

file_grid=${datadir}/vgrid.d
file_topo=${datadir}/topo.d

linearx=.true.
lineary=.false.
linearz=.false.

# input file

split=.false.
file_in=${indir}/restart.${extin}
file_in_u=${indir}/u.${extin}
file_in_v=${indir}/v.${extin}
file_in_t=${indir}/t.${extin}
file_in_s=${indir}/s.${extin}
file_in_h=${indir}/h.${extin}
file_in_avd=${indir}/avd.${extin}
file_in_avm=${indir}/avm.${extin}
file_in_avq=${indir}/avq.${extin}
file_in_eb=${indir}/eb.${extin}

# output file

grads_file_u=${outdir}/rs_u.${extgd}
grads_file_v=${outdir}/rs_v.${extgd}
grads_file_t=${outdir}/rs_t.${extgd}
grads_file_s=${outdir}/rs_s.${extgd}
grads_file_h=${outdir}/rs_h.${extgd}
grads_file_um=${outdir}/rs_um.${extgd}
grads_file_vm=${outdir}/rs_vm.${extgd}
grads_file_avd=${outdir}/rs_avd.${extgd}
grads_file_avm=${outdir}/rs_avm.${extgd}
grads_file_avq=${outdir}/rs_avq.${extgd}
grads_file_eb=${outdir}/rs_eb.${extgd}

# GrADs CTL file

ctl_true_or_false=.true.
#ctl_true_or_false=.false.
grads_ctl_file_u=./rs_u.ctl
grads_ctl_file_v=./rs_v.ctl
grads_ctl_file_t=./rs_t.ctl
grads_ctl_file_s=./rs_s.ctl
grads_ctl_file_h=./rs_h.ctl
grads_ctl_file_um=./rs_um.ctl
grads_ctl_file_vm=./rs_vm.ctl
grads_ctl_file_avd=./rs_avd.ctl
grads_ctl_file_avm=./rs_avm.ctl
grads_ctl_file_avq=./rs_avq.ctl
grads_ctl_file_eb=./rs_eb.ctl

./mkgrads_ocean_restart <<EOF
 &nocrst
  file_grid="${file_grid}",
  file_topo="${file_topo}",
  lsplit=${split}
  file_in="${file_in}",
  file_in_u="${file_in_u}",
  file_in_v="${file_in_v}",
  file_in_t="${file_in_t}",
  file_in_s="${file_in_s}",
  file_in_h="${file_in_h}",
  file_in_avd="${file_in_avd}",
  file_in_avm="${file_in_avm}",
  file_in_avq="${file_in_avq}",
  file_in_eb="${file_in_eb}",
  file_out_u="${grads_file_u}",
  file_out_v="${grads_file_v}",
  file_out_t="${grads_file_t}",
  file_out_s="${grads_file_s}",
  file_out_h="${grads_file_h}",
  file_out_um="${grads_file_um}",
  file_out_vm="${grads_file_vm}",
  file_out_avd="${grads_file_avd}",
  file_out_avm="${grads_file_avm}",
  file_out_avq="${grads_file_avq}",
  file_out_eb="${grads_file_eb}",
  file_ctl_u="${grads_ctl_file_u}",
  file_ctl_v="${grads_ctl_file_v}",
  file_ctl_t="${grads_ctl_file_t}",
  file_ctl_s="${grads_ctl_file_s}",
  file_ctl_h="${grads_ctl_file_h}",
  file_ctl_um="${grads_ctl_file_um}",
  file_ctl_vm="${grads_ctl_file_vm}",
  file_ctl_avd="${grads_ctl_file_avd}",
  file_ctl_avm="${grads_ctl_file_avm}",
  file_ctl_avq="${grads_ctl_file_avq}",
  file_ctl_eb="${grads_ctl_file_eb}",
  inum=${nrst},
  undef=0.0e1,
  lmakectl=${ctl_true_or_false},
  linear_x=${linearx},
  linear_y=${lineary},
  linear_z=${linearz}
 /
EOF
