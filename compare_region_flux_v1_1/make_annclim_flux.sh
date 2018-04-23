#!/bin/bash

set -e

if [ x${5} = x ]; then
  echo "Usage: ${0} item version region_number start_year end_year"
  exit
fi

item=${1}
version=${2}
region_number=${3}
yearst=${4}
yeared=${5}

########################################

file_region[1]=arctic
file_region[2]=subpnatl
file_region[3]=subanatl
file_region[4]=subtnatl
file_region[5]=tropnatl
file_region[6]=tropsatl
file_region[7]=subtsatl
file_region[8]=subpsatl
file_region[9]=tropind
file_region[10]=noname           # a ... blank
file_region[11]=weddell          # b
file_region[12]=soatl            # c
file_region[13]=troppac          # d
file_region[14]=subtsind         # e
file_region[15]=soind            # f
file_region[16]=subtnpac         # g
file_region[17]=tropnpac         # h
file_region[18]=subtspac         # i
file_region[19]=sopac            # j
file_region[20]=subpnpac         # k
file_region[21]=mediterranean    # l
file_region[22]=black            # m

########################################

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

file_base=${item}-${version}-${file_region[${region_number}]}
fileo=${item}-${version}-${file_region[${region_number}]}_mean_${yearst}_${yeared}.txt

l2d=.true.
cgrid=U

exe=annclim_ctl

./${exe}<<EOF
  &nml_annclim
    file_base='${file_base}'
    dir_out='.'
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
