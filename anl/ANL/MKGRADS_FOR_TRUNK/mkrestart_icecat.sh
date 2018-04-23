#!/bin/bash

extin=${1}
extgd=${2}

if [ x${extgd} = x ]; then
  echo " Usage : mkrestart_sicat.sh inputfile_extension outputfile_extension "
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

file_sicat=${indir}/sicatrsta.${extin}
grads_sicat=${outdir}/rs_icecat.${extgd}

echo "${file_sicat}"

ctl_true_or_false=.true.
#ctl_true_or_false=.false.
#fname_ctl_ts=${outdir}/hs_icecat_ts.ctl
#fname_ctl_uv=${outdir}/hs_icecat_uv.ctl

fname_ctl_ts=./rs_icecat_ts.ctl
fname_ctl_uv=./rs_icecat_uv.ctl

./mkgrads_icecat_restart <<EOF
 &nictrst
  file_grid="${file_grid}",
  file_topo="${file_topo}",
  file_in="${file_sicat}",
  file_out="${grads_sicat}",
  file_ctl_ts="${fname_ctl_ts}",
  file_ctl_uv="${fname_ctl_uv}",
  lmakectl=${ctl_true_or_false},
  linear_x=${linearx},
  linear_y=${lineary},
  linear_z=${linearz}
 /
EOF
