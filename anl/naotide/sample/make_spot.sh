#!/bin/bash


ln -sf test/glb/namelist.configure.in .

fileo=temp

exec=spot_ctl
make ${exec}

./${exec}<<EOF
  &spot_lst
    lon=140.d0,
    lat=20.d0,
    fileo="${fileo}",
  /
EOF

exit 0
