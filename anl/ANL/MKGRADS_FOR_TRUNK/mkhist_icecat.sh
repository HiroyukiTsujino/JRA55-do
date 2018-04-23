#!/bin/bash

extin=${1}
extgd=${2}

if [ x${extgd} = x ]; then
  echo " Usage : mkhist_sicat.sh inputfile_extension outputfile_extension "
  exit
fi

indir=../run/result
outdir=../run/result

datadir=../data

# define grid 

file_grid=${datadir}/vgrid.d
file_topo=${datadir}/topo.d

linearx=.true.
lineary=.false.
linearz=.false.

nhist=1 # one file contains one data

file_sicat=${indir}/sicathsta.${extin}
grads_sicat_ts=${outdir}/hs_icecat_ts.${extgd}
grads_sicat_uv=${outdir}/hs_icecat_uv.${extgd}

echo "${file_sicat}"

ctl_true_or_false=.true.
#ctl_true_or_false=.false.
#fname_ctl_ts=${outdir}/hs_icecat_ts.ctl
#fname_ctl_uv=${outdir}/hs_icecat_uv.ctl

fname_ctl_ts=./hs_icecat_ts.ctl
fname_ctl_uv=./hs_icecat_uv.ctl

./mkgrads_icecat_hist <<EOF
 &nicecat
  file_grid="${file_grid}",
  file_topo="${file_topo}",
  inum=${nhist},
  file_in="${file_sicat}",
  file_out_ts="${grads_sicat_ts}",
  file_out_uv="${grads_sicat_uv}",
  file_ctl_ts="${fname_ctl_ts}",
  file_ctl_uv="${fname_ctl_uv}",
  title_ts=" OImon (TS) ",
  title_uv=" OImon (UV) ",
  lmakectl=${ctl_true_or_false},
  linear_x=${linearx},
  linear_y=${lineary},
  linear_z=${linearz}
 /
EOF
