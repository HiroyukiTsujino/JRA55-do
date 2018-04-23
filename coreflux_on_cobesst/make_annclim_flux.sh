#!/bin/bash

set -e

if [ x${3} = x ]; then
  echo "Usage: ${0} item start_year end_year"
  exit
fi

item=${1}
yearst=${2}
yeared=${3}

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

dsmonth=aug2017
#dsmonth=jan2017
#dsmonth=dec2015

file_base=${dsmonth}/${item}_glb_${dsmonth}

fileo=${item}_mean_${yearst}_${yeared}.txt

l2d=.true.
cgrid=U

exe=annclim_ctl

./${exe}<<EOF
  &nml_annclim
    file_base='${file_base}'
    dir_out='annclim_${dsmonth}'
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
