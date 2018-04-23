#!/bin/bash

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year"
  exit
fi

yearst=${1}
yeared=${2}

ln -sf NAMELIST.MXE.GONDOLA_100.annual NAMELIST.MXE

file_base=/work116/htsujino/SURF_FLUX/reference_simulation/GONDOLA_100-run-20161116j/grdann_real/hs_siflux_int
fileo=hs_siflux_int-${yearst}_${yeared}.txt
dir_out=gondola_100_20161116j

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
    kmax=4
  /
EOF

exit 0
