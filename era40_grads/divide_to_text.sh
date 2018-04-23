#!/bin/bash

set -e

ln -sf NAMELIST.MXE.ERA40.annual_ocean NAMELIST.MXE # ocean only

exe=mk_mask_divide

cgrid=U

#indxdir=60s40s
#slat=-60.0d0
#elat=-40.0d0
#slon=0.0d0
#elon=360.0d0

#indxdir=40s20s
#slat=-40.0d0
#elat=-20.0d0
#slon=0.0d0
#elon=360.0d0

#indxdir=20s0s
#slat=-20.0d0
#elat=0.0d0
#slon=0.0d0
#elon=360.0d0

#indxdir=0n20n
#slat=0.0d0
#elat=20.0d0
#slon=0.0d0
#elon=360.0d0

#indxdir=20n40n
#slat=20.0d0
#elat=40.0d0
#slon=0.0d0
#elon=360.0d0

#indxdir=40n60n
#slat=40.0d0
#elat=60.0d0
#slon=0.0d0
#elon=360.0d0

#indxdir=tropics
#slat=-20.0d0
#elat=20.0d0
#slon=0.0d0
#elon=360.0d0

#indxdir=so
#slat=-80.0d0
#elat=-30.0d0
#slon=0.0d0
#elon=360.0d0

#indxdir=nh
#slat=20.0d0
#elat=70.0d0
#slon=0.0d0
#elon=360.0d0

#indxdir=np
#slat=70.0d0
#elat=90.0d0
#slon=0.0d0
#elon=360.0d0

#indxdir=tio
#slat=-10.0d0
#elat=10.0d0
#slon=40.0d0
#elon=100.0d0

#indxdir=pac_sv
#slat=5.0d0
#elat=65.0d0
#slon=115.0d0
#elon=285.0d0

#indxdir=basin_map
#slat=65.0d0
#elat=90.0d0
#slon=0.0d0
#elon=360.0d0

#indxdir=ocean
#slat=-90.0d0
#elat=90.0d0
#slon=0.0d0
#elon=360.0d0

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
