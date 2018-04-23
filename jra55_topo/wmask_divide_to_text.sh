#!/bin/bash

exe=mk_wind_fill_divide

cgrid=U

indxdir=wind_fill_org
slat=-90.0d0
elat=90.0d0
slon=0.0d0
elon=360.0d0

if [ -e ${indxdir} ]; then
  echo "${indxdir} already exists, please check"
  exit
else
  mkdir ${indxdir}
fi

./${exe}<<EOF
&nml_mkmask
  slat=${slat},
  elat=${elat},
  slon=${slon},
  elon=${elon},
  cgrid="${cgrid}",
  cdir="${indxdir}"
  file_wind_mask="/worke/htsujino/QuikSCAT/L3/jpl/v2/grads_monthly_on_jra55/jra55_qscat_mask.gd"
/
EOF

exit 0
