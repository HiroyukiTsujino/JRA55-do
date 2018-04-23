#!/bin/bash

file_list=test/file_list.txt
fileo=dummy

exe=mean_ctl
make ${exe}
if [ $? -ne 0 ]; then
  echo Error: MAKE failed for ${exe}
  exit 1
fi

./${exe}<<EOF
  &mean_lst
    file_list="${file_list}",
    fileo="${fileo}",
    mx_str=1,
    mx_end=2,
    my_str=1,
    my_end=2,
    lgrads=.true.,
  /
EOF

exit 0
