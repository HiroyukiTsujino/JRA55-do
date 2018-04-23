#!/bin/bash

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year"
  exit
fi

yearst=${1}
yeared=${2}

ln -sf NAMELIST.MXE.CORE2g.annual NAMELIST.MXE

file_base=/work116/htsujino/SURF_FLUX/reference_simulation/CORE2g-run-20130808/annual_mean/hs_q_component
fileo=hs_q_component-${yearst}_${yeared}.txt
dir_out=core2g_20130808

l2d=.true.
cgrid=U

exe=annclim_ctl

./${exe}<<EOF
  &nml_annclim
    file_base='${file_base}'
    dir_out='${dir_out}'
    file_out='${fileo}'
    l_one_record=.true.
    l_read_from_onefile=.false.
    l_write_to_text=.true.
    undef_out=9.999e20
    yearstr=${yearst}
    yearend=${yeared}
    yoffset=0
    tuxy='t'
    kmax=9
  /
EOF

exit 0
