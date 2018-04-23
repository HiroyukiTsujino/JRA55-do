#!/bin/bash

. ../../setting/set_environment.sh

ln -sf test/namelist.configure.in.glb namelist.configure.in

diro=${rootpath_data}/temp
if [ ! -d ${diro} ]; then
  mkdir ${diro}
fi


exe=tidehmap_ctl
make ${exe}


./${exe}<<EOF
  &tidehmap_lst
    diro="${diro}",
    ltopo=.true.,
  /
EOF

mv naoh.ctl ${diro}/

exit 0
