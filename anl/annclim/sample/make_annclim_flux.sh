#!/bin/bash

set -e

if [ x${4} = x ]; then
  echo "Usage: ${0} item version start_year end_year"
  exit
fi

item=${1}
version=${2}
yearst=${3}
yeared=${4}

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

file_base=${item}_${version}
fileo=${item}_mean_${yearst}_${yeared}.txt

l2d=.true.
cgrid=U

exe=annclim_ctl

./${exe}<<EOF
  &nml_annclim
    file_base='${file_base}'
    dir_out='${version}'
    file_out='${fileo}'
    l_one_record=.true.
    l_read_from_onefile=.true.
    l_write_to_text=.true.
    undef_out=9.999e20
    yearstr=${yearst}
    yearend=${yeared}
    yoffset=0
    tuxy='u'
    kmax=1
  /
EOF

exit 0
