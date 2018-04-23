#!/bin/bash


ln -sf test/jpn/namelist.configure.in .
ln -sf test/jpn/namelist.test .

ln -sf test/jpn/namelist.configure.in namelist.configure.in.ave

ln -sf ../../linkdir/mxe/data/tide/japan_coast .
ln -sf ../../linkdir/mxe/data/tide/japan_coast/stationf.tbl .


dir=../../linkdir/mxe/jpn/snp

driver=cap_jcoast_driver
make ${driver} || exit 1

./${driver}<<EOF || exit 1
  &cap_jcoast_lst
    file_base="${dir}/hs_snp_ssh",
    file_base_ave="${dir}/ssh_25h",
    nstn=1,
  /
EOF

rm japan_coast namelist.configure.in.ave stationf.tbl

exit 0
