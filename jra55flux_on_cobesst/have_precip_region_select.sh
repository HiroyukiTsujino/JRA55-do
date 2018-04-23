#!/bin/bash

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} region_number (1-21: except for 10)"
  exit
fi

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

#file_base=../linkdir/forcing/jra55fcst_v7_rad1_annual_1x1/precip
#fileo=precip_med_v0_7_1

region_number=${1}

file_region[1]=arctic
file_region[2]=subpnatl
file_region[3]=subanatl
file_region[4]=subtnatl
file_region[5]=tropnatl
file_region[6]=tropsatl
file_region[7]=subtsatl
file_region[8]=subpsatl
file_region[9]=tropind
file_region[10]=noname
file_region[11]=weddell
file_region[12]=soatl
file_region[13]=troppac
file_region[14]=subtsind
file_region[15]=soind
file_region[16]=subtnpac
file_region[17]=tropnpac
file_region[18]=subtnpac
file_region[19]=sopac
file_region[20]=mediterranean
file_region[21]=mediterranean
file_region[22]=black

#fileo=precip_arctic_v0_7_2
#region_number=1.0

#fileo=precip_subpolatl_v0_7_2
#region_number=2.0

#fileo=precip_subarcatl_v0_7_2
#region_number=3.0

#file_base=../linkdir/forcing/jra55fcst_v1_1_annual_1x1/precip
#fileo=precip_med_v1_1

file_base=../linkdir/forcing/jra55fcst_v1_1_annual_1x1/precip
fileo=precip-v1_1-${file_region[${region_number}]}

echo ${fileo}

#exit

file_mask=/work116/htsujino/COBESST/data/region_index_fulldiv.gd
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
    i_region_number=${region_number}
  /
EOF

exit 0
