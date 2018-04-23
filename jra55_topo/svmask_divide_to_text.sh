#!/bin/bash

exe=mk_wind_sv_divide

cgrid=U

indxdir=sv_org

if [ -e ${indxdir} ]; then
  echo "${indxdir} already exists, please check"
  exit
else
  mkdir ${indxdir}
fi

./${exe}<<EOF
&nml_mksvmask_div
  cgrid="${cgrid}",
  cdir="${indxdir}"
/
EOF

exit 0
