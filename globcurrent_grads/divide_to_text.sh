#!/bin/bash

set -e

ln -sf NAMELIST.MXE.GlobCurrent.topo NAMELIST.MXE

exe=mk_mask_divide

cgrid=U

indxdir=ocean
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
/
EOF

rm -f NAMELIST.MXE

exit 0
