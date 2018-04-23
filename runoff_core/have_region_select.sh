#!/bin/bash

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} item region_number"
  echo "  item = runoff"
  echo "  region_number = 1-21 (except for 10)"
  exit
fi

item=${1}
item_out=${item}_all
region_number=${2}

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

file_in_base=/work116/htsujino/CORE/core_river_annual_cobesst
vername=core

file_out_base=fulldiv-${vername}
file_mask=/work116/htsujino/COBESST/data/region_index_fulldiv.gd
l2d=.true.
cgrid=U

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

file_base=${file_in_base}/${item}

fileo=${item_out}-${vername}-${file_region[${region_number}]}
filectl=${item_out}-${vername}-${file_region[${region_number}]}.ctl

echo ${fileo} ${filectl}

exe=have_ctl

./${exe}<<EOF
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=${l2d},
    cgrid="${cgrid}",
    file_mask="${file_mask}"
    i_region_number=${region_number}
  /
EOF

sed -e s/%y4/gd/ \
    -e s/template// \
    -e s/365_day_calendar// \
${filectl} > tmp.ctl

mv tmp.ctl ${filectl}

if [ ! -e ${file_out_base} ]; then
  mkdir -p ${file_out_base}
fi

mv ${fileo}.gd  ${file_out_base}/.
mv ${filectl}   ${file_out_base}/.

exit 0
