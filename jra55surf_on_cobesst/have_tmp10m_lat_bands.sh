#!/bin/bash

set -e

if [ x${1} = x ]; then
  echo "Usage : ${0} mask_name"
  exit  
fi

index=${1}

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/forcing/jra55fcst_v1_1_annual_1x1/tmp10m
#fileo=tmp10m_${index}_v1_1

file_base=../linkdir/forcing/jra55fcst_annual_1x1/tmp10m
fileo=tmp10m_${index}_v0_1

file_mask=/work116/htsujino/COBESST/data/${index}_mask.gd
l2d=.true.
cgrid=U
exe=have_ctl

./${exe}<<EOF
  &have_lst
    file_base="${file_base}",
    fileo="${fileo}",
    l2d=${l2d},
    cgrid="${cgrid}",
    file_mask="${file_mask}"
  /
EOF

exit 0
